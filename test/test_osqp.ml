let settings = { Osqp.default_settings with verbose = false }

let test_problem () =
  let p = [| [| 4.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  let q = [| 1.0; 1.0 |] in
  let a = [| [| 1.0; 1.0 |]; [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let l = [| 1.0; 0.0; 0.0 |] in
  let u = [| 1.0; 0.7; 0.7 |] in
  (p, q, a, l, u)

let unwrap = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "unexpected error"

let test_solve () =
  let p, q, a, l, u = test_problem () in
  let t = Osqp.setup ~settings ~p ~q ~a ~l ~u () |> unwrap in
  let sol = Osqp.solve t |> unwrap in
  Alcotest.(check (float 1e-2)) "x[0]" 0.3 sol.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" 0.7 sol.x.(1)

let test_warm_start () =
  let p, q, a, l, u = test_problem () in
  let t = Osqp.setup ~settings ~p ~q ~a ~l ~u () |> unwrap in
  let sol = Osqp.solve t |> unwrap in
  Osqp.warm_start t ~x:sol.x ~y:sol.y () |> unwrap;
  let sol2 = Osqp.solve t |> unwrap in
  Alcotest.(check (float 1e-2)) "x[0]" sol.x.(0) sol2.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" sol.x.(1) sol2.x.(1)

let test_update_vectors () =
  let p, q, a, l, u = test_problem () in
  let t = Osqp.setup ~settings ~p ~q ~a ~l ~u () |> unwrap in
  let _ = Osqp.solve t |> unwrap in
  Osqp.update_vectors t ~q:[| 2.0; 1.0 |] () |> unwrap;
  let sol = Osqp.solve t |> unwrap in
  Alcotest.(check (float 1e-2)) "x[0]" 0.3 sol.x.(0);
  Alcotest.(check (float 1e-2)) "x[1]" 0.7 sol.x.(1)

let test_update_matrices () =
  let p, q, a, l, u = test_problem () in
  let t = Osqp.setup ~settings ~p ~q ~a ~l ~u () |> unwrap in
  let _ = Osqp.solve t |> unwrap in
  let p' = [| [| 6.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  Osqp.update_matrices t ~p:p' () |> unwrap;
  let sol = Osqp.solve t |> unwrap in
  Alcotest.(check bool) "solution non-empty" true (Array.length sol.x > 0)

let () =
  Alcotest.run "OSQP"
    [
      ("solve", [ Alcotest.test_case "basic" `Quick test_solve ]);
      ("warm_start", [ Alcotest.test_case "matches initial solve" `Quick test_warm_start ]);
      ("update_vectors", [ Alcotest.test_case "change q" `Quick test_update_vectors ]);
      ("update_matrices", [ Alcotest.test_case "change p" `Quick test_update_matrices ]);
    ]
