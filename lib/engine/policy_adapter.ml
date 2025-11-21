module Make (P : Policy_sig.S) : Strategy_sig.S with type state = P.t = struct
  type state = P.t
  type env = Strategy_sig.env

  let init setup_opt = P.init_day setup_opt
  let step (_env : env) st bar = P.on_bar st bar
  let finalize_day (_env : env) st last_bar = P.on_session_end st last_bar
end
