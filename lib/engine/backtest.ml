open Core
open Types
open Csv_parser

[@@@warning "-27-32-69"]

type engine_config = {
  session_start_min : int;
  session_end_min   : int;
  trade_start_min   : int;
  trade_end_min     : int;
  build_trade_plan  : setup -> trade_plan option;
  on_plan_bar       : trade_plan -> bar_1m -> unit;
}

let noop_on_bar _ _ = ()

let record_trade trades_acc daily_pnl_tbl ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(reason : exit_reason) =
  let pnl_pts =
    match plan.direction with
    | Long  -> exit_price -. plan.entry_price
    | Short -> plan.entry_price -. exit_price
  in
  let pnl_R = pnl_pts /. plan.r_pts in
  let duration_min =
    Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day)
  in
  let t = {
    date         = exit_ts.date;
    direction    = plan.direction;
    entry_ts     = active.entry_ts;
    exit_ts;
    entry_price  = plan.entry_price;
    exit_price;
    qty          = 1.0;
    r_pts        = plan.r_pts;
    pnl_pts;
    pnl_R;
    pnl_usd      = 0.0;
    pnl_pct      = None;
    duration_min;
    exit_reason  = reason;
    meta         = [
      ("target_mult", Float.to_string plan.target_mult);
      ("abr_prev", Float.to_string plan.abr_prev);
      ("b1_range", Float.to_string plan.b1_range);
      ("b2_follow",
       (match plan.b2_follow with Follow_good -> "good" | Follow_poor -> "poor"));
    ];
  } in
  trades_acc := t :: !trades_acc;
  Hashtbl.update daily_pnl_tbl exit_ts.date ~f:(function
      | None   -> pnl_R
      | Some x -> x +. pnl_R)

let run ~(config : engine_config) filename setups_tbl
  : trade list * (Date.t * float) list =
  let trades_acc : trade list ref = ref [] in
  let daily_pnl_tbl : float Date.Table.t = Date.Table.create () in

  let current_date  : Date.t option ref = ref None in
  let trade_plan    : trade_plan option ref = ref None in
  let trade_state   : trade_state ref = ref No_trade in
  let last_session_bar  : bar_1m option ref = ref None in

  let finalize_day () =
    match !current_date, !trade_plan, !trade_state, !last_session_bar with
    | Some date, Some plan, Active active, Some last_bar
      when Date.equal last_bar.ts.date date ->
        record_trade trades_acc daily_pnl_tbl ~plan ~active
          ~exit_ts:last_bar.ts ~exit_price:last_bar.close
          ~reason:Eod_flat;
        trade_state := Done;
        trade_plan  := None;
        last_session_bar := None
    | _ ->
        trade_state := No_trade;
        trade_plan  := None;
        last_session_bar := None
  in

  let process_bar (bar : bar_1m) =
    let { ts = { date; minute_of_day } as ts; high; low; close; _ } = bar in

    (match !current_date with
     | None ->
         current_date := Some date;
         (match Hashtbl.find setups_tbl date with
          | None -> trade_plan := None; trade_state := No_trade
          | Some s ->
              (match config.build_trade_plan s with
               | None ->
                   trade_plan := None; trade_state := No_trade
               | Some plan ->
                   trade_plan := Some plan;
                   trade_state := Pending))
     | Some d when Date.equal d date -> ()
     | Some _d ->
         finalize_day ();
         current_date := Some date;
         (match Hashtbl.find setups_tbl date with
          | None -> trade_plan := None; trade_state := No_trade
          | Some s ->
              (match config.build_trade_plan s with
               | None ->
                   trade_plan := None; trade_state := No_trade
               | Some plan ->
                   trade_plan := Some plan;
                   trade_state := Pending)));

    if minute_of_day >= config.session_start_min && minute_of_day <= config.session_end_min then
      last_session_bar := Some bar;

    match !trade_plan with
    | None -> ()
    | Some plan ->
        if minute_of_day < config.trade_start_min || minute_of_day > config.trade_end_min then
          ()
        else begin
          config.on_plan_bar plan bar;

          match !trade_state with
          | No_trade | Done -> ()
          | Pending ->
              (match plan.direction with
               | Long ->
                   if Float.(high >= plan.entry_price) then
                     trade_state := Active {
                         stop_price   = plan.stop_init;
                         moved_to_be  = false;
                         entry_ts     = ts;
                       }
                   else if Float.(low <= plan.cancel_level) then
                     trade_state := Done
               | Short ->
                   if Float.(low <= plan.entry_price) then
                     trade_state := Active {
                         stop_price   = plan.stop_init;
                         moved_to_be  = false;
                         entry_ts     = ts;
                       }
                   else if Float.(high >= plan.cancel_level) then
                     trade_state := Done)
          | Active active ->
              let stopped =
                match plan.direction with
                | Long  -> Float.(low <= active.stop_price)
                | Short -> Float.(high >= active.stop_price)
              in
              if stopped then begin
                record_trade trades_acc daily_pnl_tbl ~plan ~active
                  ~exit_ts:ts ~exit_price:active.stop_price
                  ~reason:Stop;
                trade_state := Done
              end else begin
                if not active.moved_to_be then begin
                  match plan.direction with
                  | Long when Float.(high >= plan.be_trigger) ->
                      active.stop_price <- plan.entry_price;
                      active.moved_to_be <- true
                  | Short when Float.(low <= plan.be_trigger) ->
                      active.stop_price <- plan.entry_price;
                      active.moved_to_be <- true
                  | _ -> ()
                end;
                let hit_target =
                  match plan.direction with
                  | Long  -> Float.(high >= plan.target_price)
                  | Short -> Float.(low <= plan.target_price)
                in
                if hit_target then begin
                  record_trade trades_acc daily_pnl_tbl ~plan ~active
                    ~exit_ts:ts ~exit_price:plan.target_price
                    ~reason:Target;
                  trade_state := Done
                end
              end
        end
  in

  iter_bars filename ~f:process_bar;
  finalize_day ();

  let trades = List.rev !trades_acc in
  let daily_pnl =
    Hashtbl.to_alist daily_pnl_tbl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  trades, daily_pnl
