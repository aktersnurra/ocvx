open Ctypes
open Foreign
open Result.Syntax
open Osqp

let () =
  let p = [| [| 4.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  let q = [| 1.0; 1.0 |] in
  let a = [| [| 1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let l = [| 1.0; 0.0; 0.0 |] in
  let u = [| 1.0; 0.7; 0.7 |] in

  match Api.setup ~p ~q ~a ~l ~u () with
  | Error e -> Printf.printf "setup failed\n"
  | Ok t -> (
      match Api.solve t with
      | Error e -> Printf.printf "solve failed\n"
      | Ok sol ->
          Array.iteri (fun i v -> Printf.printf "x[%d] = %f\n" i v) sol.x;
          Printf.printf "PASS\n")
