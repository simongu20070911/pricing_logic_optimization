type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
}

val default_config : config

val strategy_id : string

val parameter_specs : Parameters.t list

val config_of_params : Parameters.value_map -> config

val make_strategy : config -> Engine.strategy

val strategy : Engine.strategy
