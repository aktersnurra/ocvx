let float_arr = Alcotest.(array (float 1e-10))
let int_arr = Alcotest.(array int)

let test_upper_triangular_zeros_lower () =
  let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let r = Common.upper_triangular m in
  Alcotest.check float_arr "row 0" [| 1.0; 2.0 |] r.(0);
  Alcotest.check float_arr "row 1" [| 0.0; 4.0 |] r.(1)

let test_upper_triangular_identity () =
  let m = [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  let r = Common.upper_triangular m in
  Alcotest.check float_arr "row 0" [| 1.0; 0.0 |] r.(0);
  Alcotest.check float_arr "row 1" [| 0.0; 1.0 |] r.(1)

let test_upper_triangular_3x3 () =
  let m =
    [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |]; [| 7.0; 8.0; 9.0 |] |]
  in
  let r = Common.upper_triangular m in
  Alcotest.check float_arr "row 0" [| 1.0; 2.0; 3.0 |] r.(0);
  Alcotest.check float_arr "row 1" [| 0.0; 5.0; 6.0 |] r.(1);
  Alcotest.check float_arr "row 2" [| 0.0; 0.0; 9.0 |] r.(2)

let test_upper_triangular_does_not_mutate () =
  let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let _ = Common.upper_triangular m in
  Alcotest.check float_arr "original row 0 unchanged" [| 1.0; 2.0 |] m.(0);
  Alcotest.check float_arr "original row 1 unchanged" [| 3.0; 4.0 |] m.(1)

let test_csc_identity () =
  let csc = Common.csc_of_dense [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  Alcotest.(check int) "nrows" 2 csc.nrows;
  Alcotest.(check int) "ncols" 2 csc.ncols;
  Alcotest.check float_arr "values" [| 1.0; 1.0 |] csc.values;
  Alcotest.check int_arr "row_indices" [| 0; 1 |] csc.row_indices;
  Alcotest.check int_arr "col_pointers" [| 0; 1; 2 |] csc.col_pointers

let test_csc_full () =
  let csc = Common.csc_of_dense [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  Alcotest.(check int) "nrows" 2 csc.nrows;
  Alcotest.(check int) "ncols" 2 csc.ncols;
  Alcotest.check float_arr "values" [| 1.0; 3.0; 2.0; 4.0 |] csc.values;
  Alcotest.check int_arr "row_indices" [| 0; 1; 0; 1 |] csc.row_indices;
  Alcotest.check int_arr "col_pointers" [| 0; 2; 4 |] csc.col_pointers

let test_csc_all_zeros () =
  let csc = Common.csc_of_dense [| [| 0.0; 0.0 |]; [| 0.0; 0.0 |] |] in
  Alcotest.check float_arr "values" [||] csc.values;
  Alcotest.check int_arr "row_indices" [||] csc.row_indices;
  Alcotest.check int_arr "col_pointers" [| 0; 0; 0 |] csc.col_pointers

let test_csc_single_column () =
  let csc = Common.csc_of_dense [| [| 1.0 |]; [| 0.0 |]; [| 3.0 |] |] in
  Alcotest.(check int) "nrows" 3 csc.nrows;
  Alcotest.(check int) "ncols" 1 csc.ncols;
  Alcotest.check float_arr "values" [| 1.0; 3.0 |] csc.values;
  Alcotest.check int_arr "row_indices" [| 0; 2 |] csc.row_indices;
  Alcotest.check int_arr "col_pointers" [| 0; 2 |] csc.col_pointers

let test_nnz_identity () =
  let csc = Common.csc_of_dense [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  Alcotest.(check int) "nnz" 2 (Common.nnz csc)

let test_nnz_zeros () =
  let csc = Common.csc_of_dense [| [| 0.0; 0.0 |]; [| 0.0; 0.0 |] |] in
  Alcotest.(check int) "nnz" 0 (Common.nnz csc)

let test_nnz_full () =
  let csc = Common.csc_of_dense [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  Alcotest.(check int) "nnz" 4 (Common.nnz csc)

(* check_symmetric *)

let test_check_symmetric_identity () =
  let m = [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |] |] in
  Alcotest.(check bool) "identity is symmetric" true
    (Result.is_ok (Common.check_symmetric m))

let test_check_symmetric_2x2 () =
  let m = [| [| 4.0; 1.0 |]; [| 1.0; 2.0 |] |] in
  Alcotest.(check bool) "symmetric 2x2" true
    (Result.is_ok (Common.check_symmetric m))

let test_check_symmetric_3x3 () =
  let m =
    [| [| 1.0; 2.0; 3.0 |]; [| 2.0; 4.0; 5.0 |]; [| 3.0; 5.0; 6.0 |] |]
  in
  Alcotest.(check bool) "symmetric 3x3" true
    (Result.is_ok (Common.check_symmetric m))

let test_check_symmetric_1x1 () =
  let m = [| [| 42.0 |] |] in
  Alcotest.(check bool) "1x1 trivially symmetric" true
    (Result.is_ok (Common.check_symmetric m))

let test_check_symmetric_returns_input () =
  let m = [| [| 1.0; 2.0 |]; [| 2.0; 1.0 |] |] in
  match Common.check_symmetric m with
  | Error _ -> Alcotest.fail "expected Ok"
  | Ok r -> Alcotest.(check bool) "returns same physical matrix" true (r == m)

let test_check_symmetric_not_symmetric () =
  let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  Alcotest.(check bool) "not symmetric returns Error" true
    (Result.is_error (Common.check_symmetric m))

let test_check_symmetric_error_message () =
  let m =
    [| [| 1.0; 2.0; 3.0 |]; [| 2.0; 4.0; 9.0 |]; [| 3.0; 5.0; 6.0 |] |]
  in
  match Common.check_symmetric m with
  | Ok _ -> Alcotest.fail "expected Error"
  | Error msg ->
    Alcotest.(check string) "error prefix" "not symmetric at"
      (String.sub msg 0 16)

let test_check_symmetric_within_tolerance () =
  let m = [| [| 1.0; 2.0 +. 5e-11 |]; [| 2.0; 1.0 |] |] in
  Alcotest.(check bool) "within 1e-10 tolerance is Ok" true
    (Result.is_ok (Common.check_symmetric m))

let test_check_symmetric_beyond_tolerance () =
  let m = [| [| 1.0; 2.0 +. 2e-10 |]; [| 2.0; 1.0 |] |] in
  Alcotest.(check bool) "beyond 1e-10 tolerance is Error" true
    (Result.is_error (Common.check_symmetric m))

let () =
  Alcotest.run "Common"
    [
      ( "upper_triangular",
        [
          Alcotest.test_case "zeros lower triangle" `Quick
            test_upper_triangular_zeros_lower;
          Alcotest.test_case "identity unchanged" `Quick
            test_upper_triangular_identity;
          Alcotest.test_case "3x3 matrix" `Quick test_upper_triangular_3x3;
          Alcotest.test_case "does not mutate input" `Quick
            test_upper_triangular_does_not_mutate;
        ] );
      ( "csc_of_dense",
        [
          Alcotest.test_case "identity" `Quick test_csc_identity;
          Alcotest.test_case "full matrix" `Quick test_csc_full;
          Alcotest.test_case "all zeros" `Quick test_csc_all_zeros;
          Alcotest.test_case "single column" `Quick test_csc_single_column;
        ] );
      ( "nnz",
        [
          Alcotest.test_case "identity" `Quick test_nnz_identity;
          Alcotest.test_case "all zeros" `Quick test_nnz_zeros;
          Alcotest.test_case "full matrix" `Quick test_nnz_full;
        ] );
      ( "check_symmetric",
        [
          Alcotest.test_case "identity" `Quick test_check_symmetric_identity;
          Alcotest.test_case "2x2" `Quick test_check_symmetric_2x2;
          Alcotest.test_case "3x3" `Quick test_check_symmetric_3x3;
          Alcotest.test_case "1x1 trivially symmetric" `Quick
            test_check_symmetric_1x1;
          Alcotest.test_case "returns input matrix" `Quick
            test_check_symmetric_returns_input;
          Alcotest.test_case "not symmetric" `Quick
            test_check_symmetric_not_symmetric;
          Alcotest.test_case "error message format" `Quick
            test_check_symmetric_error_message;
          Alcotest.test_case "within tolerance" `Quick
            test_check_symmetric_within_tolerance;
          Alcotest.test_case "beyond tolerance" `Quick
            test_check_symmetric_beyond_tolerance;
        ] );
    ]
