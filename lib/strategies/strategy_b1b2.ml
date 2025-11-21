open Core
open Types
open Time_utils
module CM = Cost_model

module Setup_builder = Setup_builder_b1b2

[@@@warning "-27-32-69"]

type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
}

let default_config = {
  session_start_min = rth_start_min;
  session_end_min = rth_end_min;
  qty = 1.0;
  cost = {
    tick_size = 0.25;
    tick_value = 12.5;
    slippage_roundtrip_ticks = 1.0;
    fee_per_contract = 4.0;
    equity_base = None;
  };
}

let strategy_id = "b1b2"

let parameter_specs =
  [
    Parameters.make
      ~name:"session_start_min"
      ~default:(Float.of_int rth_start_min)
      ~bounds:(0., 24. *. 60.)
      ~integer:true
      ~description:"session start minute-of-day (ET) used for entry gating"
      ();
    Parameters.make
      ~name:"session_end_min"
      ~default:(Float.of_int rth_end_min)
      ~bounds:(0., 24. *. 60.)
      ~integer:true
      ~description:"session end minute-of-day (ET) used for entry gating"
      ();
    Parameters.make
      ~name:"qty"
      ~default:1.0
      ~bounds:(0.1, 20.)
      ~description:"contracts per trade" ();
    Parameters.make ~name:"cost.slippage_roundtrip_ticks" ~default:1.0 ~bounds:(0., 3.)
      ~description:"assumed round-trip slippage in ticks" ();
    Parameters.make ~name:"cost.fee_per_contract" ~default:4.0 ~bounds:(0., 10.)
      ~description:"exchange+broker fee per contract" ();
    Parameters.make ~name:"cost.equity_base" ~default:0.0 ~bounds:(0., 5_000_000.)
      ~description:"account equity in USD for pct PnL; 0 => disabled" ();
  ]

let config_of_params (m : Parameters.value_map) : config =
  let get name default =
    Map.find m name |> Option.value ~default
  in
  let ss = get "session_start_min" (Float.of_int default_config.session_start_min) |> Int.of_float in
  let se = get "session_end_min" (Float.of_int default_config.session_end_min) |> Int.of_float in
  let qty = get "qty" default_config.qty in
  let cost : Cost_model.config =
    {
      tick_size = default_config.cost.tick_size;
      tick_value = default_config.cost.tick_value;
      slippage_roundtrip_ticks = get "cost.slippage_roundtrip_ticks" default_config.cost.slippage_roundtrip_ticks;
      fee_per_contract = get "cost.fee_per_contract" default_config.cost.fee_per_contract;
      equity_base =
        let eb = get "cost.equity_base" 0.0 in
        if Float.(eb <= 0.) then None else Some eb;
    }
  in
  {
    session_start_min = Int.clamp_exn ~min:0 ~max:1439 ss;
    session_end_min   = Int.clamp_exn ~min:0 ~max:1439 se;
    qty;
    cost;
  }

let record_trade ~(cfg : config) ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(reason : exit_reason) =
  let pnl_pts =
    match plan.direction with
    | Long  -> exit_price -. plan.entry_price
    | Short -> plan.entry_price -. exit_price
  in
  let pnl_R = pnl_pts /. plan.r_pts in
  let duration_min = Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day) in
  let t = {
    date         = exit_ts.date;
    direction    = plan.direction;
    entry_ts     = active.entry_ts;
    exit_ts;
    entry_price  = plan.entry_price;
    exit_price;
    qty          = cfg.qty;
    r_pts        = plan.r_pts;
    pnl_pts;
    pnl_R;
    pnl_usd      = 0.0;
    pnl_pct      = None;
    duration_min;
    exit_reason  = reason;
    meta         = [
      ("strategy", strategy_id);
      ("target_mult", Float.to_string plan.target_mult);
      ("abr_prev", Float.to_string plan.abr_prev);
      ("b1_range", Float.to_string plan.b1_range);
      ("b2_follow", (match plan.b2_follow with Follow_good -> "good" | Follow_poor -> "poor"));
    ];
  } in
  CM.apply ~qty:cfg.qty cfg.cost t

module Make (Cfg : sig val cfg : config end) = struct
  module Policy : Policy_sig.S = struct
    type t = {
      plan : trade_plan option;
      trade_state : trade_state;
    }

    let init_day setup_opt =
      match setup_opt with
      | None -> { plan = None; trade_state = No_trade }
      | Some s ->
          (match Trade_logic.build_trade_plan s with
           | None -> { plan = None; trade_state = No_trade }
           | Some plan -> { plan = Some plan; trade_state = Pending })

    let on_bar state (bar : bar_1m) : t * trade list =
      let minute_of_day = bar.ts.minute_of_day in
      match state.plan with
      | None -> (state, [])
      | Some plan ->
          if minute_of_day < b2_min || minute_of_day > Cfg.cfg.session_end_min then
            (state, [])
          else begin
            if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0) && minute_of_day > plan.b2_end_minute then begin
              plan.target_mult <- 1.0;
              plan.target_price <-
                (match plan.direction with
                 | Long  -> plan.entry_price +. plan.r_pts
                 | Short -> plan.entry_price -. plan.r_pts)
            end;

            match state.trade_state with
            | No_trade | Done -> (state, [])
            | Pending ->
                (match plan.direction with
                 | Long ->
                     if Float.(bar.high >= plan.entry_price) then
                       ({ plan = state.plan;
                          trade_state = Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts }}, [])
                     else if Float.(bar.low <= plan.cancel_level) then
                       ({ plan = state.plan; trade_state = Done }, [])
                     else (state, [])
                 | Short ->
                     if Float.(bar.low <= plan.entry_price) then
                       ({ plan = state.plan;
                          trade_state = Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts }}, [])
                     else if Float.(bar.high >= plan.cancel_level) then
                       ({ plan = state.plan; trade_state = Done }, [])
                     else (state, []))
            | Active active ->
                let stopped =
                  match plan.direction with
                  | Long  -> Float.(bar.low <= active.stop_price)
                  | Short -> Float.(bar.high >= active.stop_price)
                in
                if stopped then
                  let trade = record_trade ~cfg:Cfg.cfg ~plan ~active ~exit_ts:bar.ts ~exit_price:active.stop_price ~reason:Stop in
                  ({ plan = state.plan; trade_state = Done }, [trade])
                else begin
                  if not active.moved_to_be then begin
                    match plan.direction with
                    | Long when Float.(bar.high >= plan.be_trigger) -> active.stop_price <- plan.entry_price; active.moved_to_be <- true
                    | Short when Float.(bar.low <= plan.be_trigger) -> active.stop_price <- plan.entry_price; active.moved_to_be <- true
                    | _ -> ()
                  end;
                  let hit_target =
                    match plan.direction with
                    | Long  -> Float.(bar.high >= plan.target_price)
                    | Short -> Float.(bar.low <= plan.target_price)
                  in
                  if hit_target then
                    let trade = record_trade ~cfg:Cfg.cfg ~plan ~active ~exit_ts:bar.ts ~exit_price:plan.target_price ~reason:Target in
                    ({ plan = state.plan; trade_state = Done }, [trade])
                  else
                    ({ plan = state.plan; trade_state = Active active }, [])
                end
          end

    let on_session_end state last_bar =
      match state.plan, state.trade_state, last_bar with
      | Some plan, Active active, Some lb ->
          let trade =
          record_trade ~cfg:Cfg.cfg ~plan ~active
            ~exit_ts:lb.ts ~exit_price:lb.close ~reason:Eod_flat
          in
          ({ plan = state.plan; trade_state = Done }, [ trade ])
      | _ -> (state, [])
  end

  let strategy : Engine.strategy = {
    id = strategy_id;
    session_start_min = Cfg.cfg.session_start_min;
    session_end_min   = Cfg.cfg.session_end_min;
    build_setups = Setup_builder.compute_daily_context_and_setups;
    policy = (module Policy);
  }
end

let make_strategy cfg =
  let module M = Make(struct let cfg = cfg end) in
  M.strategy

let strategy = make_strategy default_config
