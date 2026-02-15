open Ctypes
open Foreign

let osqp_version = foreign "osqp_version" (void @-> returning string)
let osqp_settings_new = foreign "OSQPSettings_new" (void @-> returning (ptr void))
let osqp_settings_free = foreign "OSQPSettings_free" (ptr void @-> returning void)
let osqp_set_default_settings = foreign "osqp_set_default_settings" (ptr void @-> returning void)

let osqp_csc_matrix_new = foreign "OSQPCscMatrix_new"
  (int64_t @-> int64_t @-> int64_t @->
   ptr double @-> ptr int64_t @-> ptr int64_t @->
   returning (ptr void))

let osqp_csc_matrix_free = foreign "OSQPCscMatrix_free" (ptr void @-> returning void)

(* Helper: OCaml float array -> C double pointer *)
let floats arr =
  let ca = CArray.of_list double (Array.to_list arr) in
  ca, CArray.start ca

(* Helper: OCaml int array -> C int64 pointer *)
let ints arr =
  let ca = CArray.of_list int64_t (List.map Int64.of_int (Array.to_list arr)) in
  ca, CArray.start ca

let () =
  Printf.printf "OSQP version: %s\n" (osqp_version ());

  (* P = [4 1; 1 2], upper triangular only: [4; 1 2] *)
  let p_x, p_x_ptr = floats [| 4.0; 1.0; 2.0 |] in
  let p_i, p_i_ptr = ints [| 0; 0; 1 |] in
  let p_p, p_p_ptr = ints [| 0; 1; 3 |] in

  let p = osqp_csc_matrix_new 2L 2L 3L p_x_ptr p_i_ptr p_p_ptr in
  Printf.printf "P matrix allocated: %b\n" (not (is_null p));

  osqp_csc_matrix_free p;
  Printf.printf "P matrix freed\n";

  (* keep CArrays alive until here *)
  ignore (p_x, p_i, p_p)
