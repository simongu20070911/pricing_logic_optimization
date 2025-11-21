open Core
open Types
open Time_utils

module F = Features
module PS = Position_sizing
module CM = Cost_model

[@@@warning "-27-32-69"]

(* Configurable parameters *)
type config = {
  a1 : float;
  a2 : float;
  b1 : float;
  b2 : float;
  s_entry : float;
  z_exit : float;
  time_stop_min : int;
  stop_ticks : float;
  max_units : int;
  cost : Cost_model.config;
}

let default_config = {
  a1 = 1.0;
  a2 = 0.5;
  b1 = 0.5;
  b2 = 0.5;
  s_entry = 0.7;
  z_exit = 0.2;
  time_stop_min = 10;
  stop_ticks = 6.0;
  max_units = 3;
  cost = {
    tick_size = 0.25;
    tick_value = 12.5;
    (* For 3–6 ES contracts RTH on IBKR Pro, a realistic
       assumption is ~1.0 tick round‑trip slippage plus ~$4 RT fees. *)
    slippage_roundtrip_ticks = 1.0;
    fee_per_contract = 4.0;
    equity_base = None;
  };
}

let strategy_id = "vwap_revert"

let parameter_specs =
  [
    Parameters.make ~name:"a1" ~default:1.0 ~bounds:(0., 5.)
      ~description:"scale on z-score distance from VWAP" ();
    Parameters.make ~name:"a2" ~default:0.5 ~bounds:(0., 5.)
      ~description:"OFI exhaustion term weight" ();
    Parameters.make ~name:"b1" ~default:0.5 ~bounds:(0., 5.)
      ~description:"volatility penalty" ();
    Parameters.make ~name:"b2" ~default:0.5 ~bounds:(0., 5.)
      ~description:"trend penalty" ();
    Parameters.make ~name:"s_entry" ~default:0.7 ~bounds:(0.1, 3.0)
      ~description:"signal threshold to open a trade" ();
    Parameters.make ~name:"z_exit" ~default:0.2 ~bounds:(0., 1.0)
      ~description:"VWAP reversion exit band" ();
    Parameters.make ~name:"time_stop_min" ~default:10. ~bounds:(1., 120.)
      ~integer:true ~description:"time-based exit in minutes" ();
    Parameters.make ~name:"stop_ticks" ~default:6.0 ~bounds:(1., 30.)
      ~description:"hard stop distance in ticks" ();
    Parameters.make ~name:"max_units" ~default:3. ~bounds:(1., 10.)
      ~integer:true ~description:"cap on volatility-targeted units" ();
    Parameters.make ~name:"cost.slippage_roundtrip_ticks" ~default:1.0 ~bounds:(0., 3.)
      ~description:"assumed round-trip slippage in ticks" ();
    Parameters.make ~name:"cost.fee_per_contract" ~default:4.0 ~bounds:(0., 10.)
      ~description:"exchange+broker fee per contract" ();
    Parameters.make ~name:"cost.equity_base" ~default:0.0 ~bounds:(0., 5_000_000.)
      ~description:"account equity in USD for pct PnL; 0 => disabled" ();
  ]

let config_of_params (m : Parameters.value_map) : config =
  let get name default = Map.find m name |> Option.value ~default in
  let cost : CM.config =
    {
      tick_size = default_config.cost.tick_size;
      tick_value = default_config.cost.tick_value;
      slippage_roundtrip_ticks =
        get "cost.slippage_roundtrip_ticks" default_config.cost.slippage_roundtrip_ticks;
      fee_per_contract =
        get "cost.fee_per_contract" default_config.cost.fee_per_contract;
      equity_base =
        let eb = get "cost.equity_base" 0.0 in
        if Float.(eb <= 0.) then None else Some eb;
    }
  in
  {
    a1 = get "a1" default_config.a1;
    a2 = get "a2" default_config.a2;
    b1 = get "b1" default_config.b1;
    b2 = get "b2" default_config.b2;
    s_entry = get "s_entry" default_config.s_entry;
    z_exit = get "z_exit" default_config.z_exit;
    time_stop_min =
      get "time_stop_min" (Float.of_int default_config.time_stop_min)
      |> Int.of_float
      |> Int.clamp_exn ~min:1 ~max:240;
    stop_ticks = get "stop_ticks" default_config.stop_ticks;
    max_units =
      get "max_units" (Float.of_int default_config.max_units)
      |> Int.of_float
      |> Int.clamp_exn ~min:1 ~max:100;
    cost;
  }

type position =
  | Flat
  | Long_pos of { entry_ts : timestamp; entry_price : float; target_units : int }
  | Short_pos of { entry_ts : timestamp; entry_price : float; target_units : int }

type t = {
  features : F.state;
  position : position;
  cfg : config;
}

let init_state cfg = { features = F.create (); position = Flat; cfg }

let init_day cfg (_setup_opt : setup option) = init_state cfg

let compute_signal cfg (snap : F.snapshot) : float option =
  match snap.z_vwap, snap.ofi_long, snap.rv_ratio, snap.trend with
  | Some z, Some ofi_long, Some rv_ratio, Some trend ->
      let s_mr =
        let s1 = -. cfg.a1 *. z in
        let sign_ofi =
          if Float.(ofi_long > 0.) then 1.0
          else if Float.(ofi_long < 0.) then -1.0
          else 0.0
        in
        let s2 = -. cfg.a2 *. sign_ofi *. Float.abs z in
        s1 +. s2
      in
      let penalty =
        Float.exp (-. cfg.b1 *. rv_ratio) *. Float.exp (-. cfg.b2 *. trend)
      in
      Some (s_mr *. penalty)
  | _ -> None

let build_trade ~direction ~(entry_time : timestamp) ~entry_price
    ~(exit_time : timestamp) ~exit_price
    ~(reason : exit_reason) ~(target_units : int) : trade =
  let pnl_pts =
    match direction with
    | Long  -> exit_price -. entry_price
    | Short -> entry_price -. exit_price
  in
  let r_pts = 1.0 in
  let pnl_R = pnl_pts /. r_pts in
  let duration_min =
    Float.of_int (exit_time.minute_of_day - entry_time.minute_of_day)
  in
  {
    date         = exit_time.date;
    direction    = direction;
    entry_ts     = entry_time;
    exit_ts      = exit_time;
    entry_price  = entry_price;
    exit_price   = exit_price;
    qty          = Float.of_int target_units;
    r_pts        = r_pts;
    pnl_pts      = pnl_pts;
    pnl_R        = pnl_R;
    pnl_usd      = 0.0;
    pnl_pct      = None;
    duration_min = duration_min;
    exit_reason  = reason;
    meta         = [
      ("strategy", strategy_id);
      ("target_units", Int.to_string target_units);
    ];
  }

let on_bar (state : t) (bar : bar_1m) : t * trade list =
  let features = F.update state.features bar in
  let snap = F.snapshot features in
  let minute = bar.ts.minute_of_day in
  let cfg = state.cfg in
  let stop_distance_pts = cfg.stop_ticks *. cfg.cost.tick_size in

  if minute < rth_start_min || minute > rth_end_min then
    ({ features; position = Flat; cfg }, [])
  else
    let signal_opt = compute_signal cfg snap in
    match state.position, signal_opt with
    | Flat, Some s when Float.(abs s >= cfg.s_entry) ->
        let sigma = snap.rv60 in
        let target_units = PS.vol_target_units ~max:cfg.max_units ~signal:s ~sigma in
        if target_units <= 0 then
          ({ features; position = Flat; cfg }, [])
        else
        let direction = if Float.(s > 0.) then Long else Short in
        let position =
          match direction with
          | Long  -> Long_pos { entry_ts = bar.ts; entry_price = bar.close; target_units }
          | Short -> Short_pos { entry_ts = bar.ts; entry_price = bar.close; target_units }
        in
        ({ features; position; cfg }, [])
    | Flat, _ ->
        ({ features; position = Flat; cfg }, [])
    | Long_pos pos, signal_opt ->
        let entry_ts = pos.entry_ts in
        let entry_price = pos.entry_price in
        let stop_price = entry_price -. stop_distance_pts in
        let stopped = Float.(bar.low <= stop_price) in
        if stopped then
          let trade =
            build_trade
              ~direction:Long ~entry_time:entry_ts ~entry_price
              ~exit_time:bar.ts ~exit_price:stop_price ~reason:Stop
              ~target_units:pos.target_units
            |> CM.apply ~qty:(Float.of_int pos.target_units) cfg.cost
          in
          ({ features; position = Flat; cfg }, [ trade ])
        else
          let time_exit =
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.z_exit)
            | None -> false
          in
          let opp_signal =
            match signal_opt with
            | Some s when Float.(s <= 0.) -> true
            | _ -> false
          in
          if time_exit || z_revert || opp_signal then
            let trade =
              build_trade
                ~direction:Long ~entry_time:entry_ts ~entry_price
                ~exit_time:bar.ts ~exit_price:bar.close ~reason:Target
                ~target_units:pos.target_units
              |> CM.apply ~qty:(Float.of_int pos.target_units) cfg.cost
            in
            ({ features; position = Flat; cfg }, [ trade ])
          else
            ({ features; position = Long_pos pos; cfg }, [])
    | Short_pos pos, signal_opt ->
        let entry_ts = pos.entry_ts in
        let entry_price = pos.entry_price in
        let stop_price = entry_price +. stop_distance_pts in
        let stopped = Float.(bar.high >= stop_price) in
        if stopped then
          let trade =
            build_trade
              ~direction:Short ~entry_time:entry_ts ~entry_price
              ~exit_time:bar.ts ~exit_price:stop_price ~reason:Stop
              ~target_units:pos.target_units
            |> CM.apply ~qty:(Float.of_int pos.target_units) cfg.cost
          in
          ({ features; position = Flat; cfg }, [ trade ])
        else
          let time_exit =
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.z_exit)
            | None -> false
          in
          let opp_signal =
            match signal_opt with
            | Some s when Float.(s >= 0.) -> true
            | _ -> false
          in
          if time_exit || z_revert || opp_signal then
            let trade =
              build_trade
                ~direction:Short ~entry_time:entry_ts ~entry_price
                ~exit_time:bar.ts ~exit_price:bar.close ~reason:Target
                ~target_units:pos.target_units
              |> CM.apply ~qty:(Float.of_int pos.target_units) cfg.cost
            in
            ({ features; position = Flat; cfg }, [ trade ])
          else
            ({ features; position = Short_pos pos; cfg }, [])

let on_session_end (state : t) (last_bar : bar_1m option) : t * trade list =
  match state.position, last_bar with
  | Flat, _ -> (state, [])
  | (Long_pos _ | Short_pos _), None -> ({ state with position = Flat }, [])
  | Long_pos pos, Some lb ->
      let trade =
        build_trade
          ~direction:Long ~entry_time:pos.entry_ts ~entry_price:pos.entry_price
          ~exit_time:lb.ts ~exit_price:lb.close ~reason:Eod_flat
          ~target_units:pos.target_units
        |> CM.apply ~qty:(Float.of_int pos.target_units) state.cfg.cost
      in
      ({ state with position = Flat }, [ trade ])
  | Short_pos pos, Some lb ->
      let trade =
        build_trade
          ~direction:Short ~entry_time:pos.entry_ts ~entry_price:pos.entry_price
          ~exit_time:lb.ts ~exit_price:lb.close ~reason:Eod_flat
          ~target_units:pos.target_units
        |> CM.apply ~qty:(Float.of_int pos.target_units) state.cfg.cost
      in
      ({ state with position = Flat }, [ trade ])

module Make (Cfg : sig val cfg : config end) = struct
  module Policy : Policy_sig.S = struct
    type nonrec t = t
    let init_day setup = init_day Cfg.cfg setup
    let on_bar = on_bar
    let on_session_end = on_session_end
  end

  let strategy : Engine.strategy = {
    id = strategy_id;
    session_start_min = rth_start_min;
    session_end_min   = rth_end_min;
    build_setups = (fun _filename -> Date.Table.create ());
    policy = (module Policy);
  }
end

let make_strategy config =
  let module M = Make(struct let cfg = config end) in
  M.strategy
