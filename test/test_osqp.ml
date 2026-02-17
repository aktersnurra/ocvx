open Ctypes
open Foreign
open Result.Syntax

let base_problem () =
  let p = [| [| 4.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  let q = [| 1.0; 1.0 |] in
  let a = [| [| 1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let l = [| 1.0; 0.0; 0.0 |] in
  let u = [| 1.0; 0.7; 0.7 |] in
  (p, q, a, l, u)

let test_solve () =
  let p, q, a, l, u = base_problem () in
  let* t = Osqp.setup ~p ~q ~a ~l ~u () in
  let* sol = Osqp.solve t in
  Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x;
  Ok ()

let test_warm_start () =
  let p, q, a, l, u = base_problem () in
  let* t = Osqp.setup ~p ~q ~a ~l ~u () in
  let* sol = Osqp.solve t in
  let* () = Osqp.warm_start t ~x:sol.x ~y:sol.y () in
  let* sol2 = Osqp.solve t in
  Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol2.x;
  Ok ()

let test_update_vectors () =
  let p, q, a, l, u = base_problem () in
  let* t = Osqp.setup ~p ~q ~a ~l ~u () in
  let* _ = Osqp.solve t in
  let* () = Osqp.update_vectors t ~q:[| 2.0; 1.0 |] () in
  let* sol = Osqp.solve t in
  Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x;
  Ok ()

let test_update_matrices () =
  let p, q, a, l, u = base_problem () in
  let* t = Osqp.setup ~p ~q ~a ~l ~u () in
  let* _ = Osqp.solve t in
  let p' = [| [| 6.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  let* () = Osqp.update_matrices t ~p:p' () in
  let* sol = Osqp.solve t in
  Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x;
  Ok ()

let () =
  Printf.printf "\n=== Test: solve ===\n";
  (match test_solve () with
   | Error _ -> Printf.printf "FAIL\n"
   | Ok () -> Printf.printf "PASS\n");
  Printf.printf "\n=== Test: warm_start ===\n";
  (match test_warm_start () with
   | Error _ -> Printf.printf "FAIL\n"
   | Ok () -> Printf.printf "PASS\n");
  Printf.printf "\n=== Test: update_vectors ===\n";
  (match test_update_vectors () with
   | Error _ -> Printf.printf "FAIL\n"
   | Ok () -> Printf.printf "PASS\n");
  Printf.printf "\n=== Test: update_matrices ===\n";
  (match test_update_matrices () with
   | Error _ -> Printf.printf "FAIL\n"
   | Ok () -> Printf.printf "PASS\n")
