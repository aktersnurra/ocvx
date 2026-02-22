let settings = { Scs.default_settings with verbose = false }

let empty_cone : Scs.cone =
  {
    z = 0;
    l = 0;
    bu = [||];
    bl = [||];
    q = [||];
    s = [||];
    cs = [||];
    ep = 0;
    ed = 0;
    p = [||];
  }

(* Small QP from https://www.cvxgrp.org/scs/examples/c.html
   minimize    0.5 x' P x + c' x
   subject to  Ax + s = b, s in K
   where P = [[3, -1], [-1, 2]]
         A = [[-1, 1], [1, 0], [0, 1]]
         b = [-1, 0.3, -0.5]
         c = [-1, -1]
         K = {0}^1 x R+^2 *)
let test_problem () =
  let p = [| [| 3.0; -1.0 |]; [| -1.0; 2.0 |] |] in
  let a = [| [| -1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let b = [| -1.0; 0.3; -0.5 |] in
  let c = [| -1.0; -1.0 |] in
  let cone = { empty_cone with z = 1; l = 2 } in
  (p, a, b, c, cone)

let unwrap = function Ok x -> x | Error _ -> Alcotest.fail "unexpected error"

let is_solved (info : Scs.info) =
  match info.status with
  | Scs.Solved | Scs.Solved_inaccurate -> true
  | _ -> false

let test_setup () =
  let p, a, b, c, cone = test_problem () in
  let _t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  ()

let test_solve () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let sol, info = Scs.solve t () |> unwrap in
  Alcotest.(check (float 1e-2)) "x[0]" 0.3 sol.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" (-0.7) sol.x.(1);
  Alcotest.(check bool) "solved" true (is_solved info)

let test_solve_lp () =
  (* minimize  -x0 - x1
     subject to x0 + x1 <= 1, x0 >= 0, x1 >= 0
     conic form: Ax + s = b, s in R+^3 *)
  let a = [| [| 1.0; 1.0 |]; [| -1.0; 0.0 |]; [| 0.0; -1.0 |] |] in
  let b = [| 1.0; 0.0; 0.0 |] in
  let c = [| -1.0; -1.0 |] in
  let cone = { empty_cone with l = 3 } in
  let t = Scs.setup ~settings ~c ~a ~b ~cone () |> unwrap in
  let sol, info = Scs.solve t () |> unwrap in
  Alcotest.(check (float 1e-2)) "x[0]" 0.5 sol.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" 0.5 sol.x.(1);
  Alcotest.(check bool) "solved" true (is_solved info)

let test_solve_dual () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let sol, _ = Scs.solve t () |> unwrap in
  Alcotest.(check (float 1e-1)) "y[0]" 2.7 sol.y.(0);
  Alcotest.(check (float 1e-1)) "y[1]" 2.1 sol.y.(1);
  Alcotest.(check (float 1e-1)) "y[2]" 0.0 sol.y.(2)

let test_solution_dimensions () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let sol, _ = Scs.solve t () |> unwrap in
  Alcotest.(check int) "x length = n" 2 (Array.length sol.x);
  Alcotest.(check int) "y length = m" 3 (Array.length sol.y);
  Alcotest.(check int) "s length = m" 3 (Array.length sol.s)

let test_info_fields () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let _, info = Scs.solve t () |> unwrap in
  Alcotest.(check bool) "iter > 0" true (info.iter > 0);
  Alcotest.(check bool) "setup_time > 0" true (info.setup_time > 0.0);
  Alcotest.(check bool) "solve_time > 0" true (info.solve_time > 0.0);
  Alcotest.(check (float 1e-1)) "pobj" 1.235 info.pobj

let test_update_b () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let _ = Scs.solve t () |> unwrap in
  let b' = [| -1.0; 0.5; -0.5 |] in
  Scs.update t ~b:b' () |> unwrap;
  let sol, info = Scs.solve t () |> unwrap in
  Alcotest.(check bool) "solved after update b" true (is_solved info);
  Alcotest.(check (float 1e-2)) "x[0]" 0.5 sol.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" (-0.5) sol.x.(1)

let test_update_c () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let sol1, _ = Scs.solve t () |> unwrap in
  Scs.update t ~c:[| -10.0; 100.0 |] () |> unwrap;
  let sol2, info = Scs.solve ~warm_start:true ~prev_sol:sol1 t () |> unwrap in
  Printf.printf "sol1: x=[%f, %f]\n%!" sol1.x.(0) sol1.x.(1);
  Printf.printf "sol2: x=[%f, %f]\n%!" sol2.x.(0) sol2.x.(1);
  Alcotest.(check bool) "solved after update c" true (is_solved info);
  Alcotest.(check bool)
    "solution changed" true
    (Float.abs (sol1.x.(0) -. sol2.x.(0)) > 1e-4
    || Float.abs (sol1.x.(1) -. sol2.x.(1)) > 1e-4)

let test_warm_start () =
  let p, a, b, c, cone = test_problem () in
  let t = Scs.setup ~settings ~c ~a ~b ~p ~cone () |> unwrap in
  let sol1, _ = Scs.solve t () |> unwrap in
  let sol2, info2 = Scs.solve ~warm_start:true ~prev_sol:sol1 t () |> unwrap in
  Alcotest.(check bool) "solved on warm start" true (is_solved info2);
  Alcotest.(check (float 1e-2)) "x[0] matches" sol1.x.(0) sol2.x.(0);
  Alcotest.(check (float 1e-2)) "x[1] matches" sol1.x.(1) sol2.x.(1)

let () =
  Alcotest.run "SCS"
    [
      ("setup", [ Alcotest.test_case "basic" `Quick test_setup ]);
      ( "solve",
        [
          Alcotest.test_case "QP" `Quick test_solve;
          Alcotest.test_case "LP" `Quick test_solve_lp;
          Alcotest.test_case "dual variables" `Quick test_solve_dual;
          Alcotest.test_case "solution dimensions" `Quick
            test_solution_dimensions;
        ] );
      ("info", [ Alcotest.test_case "fields populated" `Quick test_info_fields ]);
      ( "update",
        [
          Alcotest.test_case "change b" `Quick test_update_b;
          Alcotest.test_case "change c" `Quick test_update_c;
        ] );
      ( "warm_start",
        [ Alcotest.test_case "reuse solution" `Quick test_warm_start ] );
    ]
