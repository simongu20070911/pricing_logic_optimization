open Core
open Strategy_fast
open Types

let find_fixture () =
  let target = Filename.concat "agents_workspace" "sample_es.csv" in
  let rec ascend n dir =
    if n < 0 then None else
      let candidate = Filename.concat dir target in
      if Stdlib.Sys.file_exists candidate then Some candidate
      else ascend (n-1) (Filename.dirname dir)
  in
  match ascend 6 (Stdlib.Sys.getcwd ()) with
  | Some p -> p
  | None -> failwith "fixture not found"

let sum_pnl trades = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R)

let%test_unit "pure strategy matches legacy engine run" =
  let file = find_fixture () in
  let res_legacy = Engine.Engine.run Strategies.Strategy_b1b2.strategy ~filename:file in
  let res_pure = Engine.Engine.run_pure Strategies.Strategy_b1b2.strategy_pure ~filename:file in
  assert (List.length res_legacy.trades = List.length res_pure.trades);
  let tol = 1e-6 in
  let diff = Float.abs (sum_pnl res_legacy.trades -. sum_pnl res_pure.trades) in
  assert (Float.(diff < tol));
  List.iter2_exn res_legacy.trades res_pure.trades ~f:(fun a b ->
      assert (Poly.(a.direction = b.direction));
      assert (Poly.(a.exit_reason = b.exit_reason));
      assert (Date.equal a.date b.date))
