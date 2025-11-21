open Core
open Types

module type POLICY = Policy_sig.S

type strategy = {
  id : string;
  session_start_min : int;
  session_end_min   : int;
  build_setups : string -> setup Date.Table.t;
  policy : (module POLICY);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

let run (strategy : strategy) ~(filename : string) : run_result =
  let setups_tbl = strategy.build_setups filename in
  let module P = (val strategy.policy : POLICY) in

  let trades_acc : trade list ref = ref [] in
  let daily_pnl_tbl : float Date.Table.t = Date.Table.create () in
  let daily_pnl_usd_tbl : float Date.Table.t = Date.Table.create () in
  let daily_pnl_pct_tbl : float Date.Table.t = Date.Table.create () in

  let current_date  : Date.t option ref = ref None in
  let policy_state  : P.t option ref = ref None in
  let last_session_bar : bar_1m option ref = ref None in

  let flush_trades trades =
    List.iter trades ~f:(fun t ->
        trades_acc := t :: !trades_acc;
        Hashtbl.update daily_pnl_tbl t.date ~f:(function
            | None -> t.pnl_R
            | Some x -> x +. t.pnl_R);
        Hashtbl.update daily_pnl_usd_tbl t.date ~f:(function
            | None -> t.pnl_usd
            | Some x -> x +. t.pnl_usd);
        (match t.pnl_pct with
         | None -> ()
         | Some pct ->
             Hashtbl.update daily_pnl_pct_tbl t.date ~f:(function
                 | None -> pct
                 | Some x -> x +. pct)))
  in

  let finalize_day () =
    match !policy_state with
    | None -> ()
    | Some st ->
        let st', trades = P.on_session_end st !last_session_bar in
        policy_state := Some st';
        flush_trades trades;
        policy_state := None;
        last_session_bar := None
  in

  Csv_parser.iter_bars filename ~f:(fun bar ->
      let { ts = { date; minute_of_day }; _ } = bar in

      (match !current_date with
       | None ->
           current_date := Some date;
           let setup_opt = Hashtbl.find setups_tbl date in
           policy_state := Some (P.init_day setup_opt)
       | Some d when Date.equal d date -> ()
       | Some _ ->
           finalize_day ();
           current_date := Some date;
           let setup_opt = Hashtbl.find setups_tbl date in
           policy_state := Some (P.init_day setup_opt));

      if minute_of_day >= strategy.session_start_min
         && minute_of_day <= strategy.session_end_min
      then last_session_bar := Some bar;

      match !policy_state with
      | None -> ()
      | Some st ->
          let st', trades = P.on_bar st bar in
          policy_state := Some st';
          flush_trades trades);

  finalize_day ();

  let trades = List.rev !trades_acc in
  let daily_pnl =
    Hashtbl.to_alist daily_pnl_tbl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_usd =
    Hashtbl.to_alist daily_pnl_usd_tbl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_pct =
    Hashtbl.to_alist daily_pnl_pct_tbl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  { setups = setups_tbl; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct }
