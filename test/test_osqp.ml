open Ctypes
open Foreign
open Osqp.Bindings


let () =
  let p_x, p_x_ptr = floats [4.0; 1.0; 2.0] in
  let p_i, p_i_ptr = ints [0L; 0L; 1L] in
  let p_p, p_p_ptr = ints [0L; 1L; 3L] in
  let p_nnz = 3L in
  let _q, q_ptr = floats [1.0; 1.0] in
  let a_x, a_x_ptr = floats [1.0; 1.0; 1.0; 1.0] in
  let a_i, a_i_ptr = ints [0L; 1L; 0L; 2L] in
  let a_p, a_p_ptr = ints [0L; 2L; 4L] in
  let a_nnz = 4L in
  let _l, l_ptr = floats [1.0; 0.0; 0.0] in
  let _u, u_ptr = floats [1.0; 0.7; 0.7] in
  let n = 2L in
  let m = 3L in

  let p = osqp_csc_matrix_new n n p_nnz p_x_ptr p_i_ptr p_p_ptr in
  let a = osqp_csc_matrix_new m n a_nnz a_x_ptr a_i_ptr a_p_ptr in

  let settings = osqp_settings_new () in
  osqp_set_default_settings settings;
  (* setf (!@ settings) alpha 1.0; *)

  let solver_ptr = allocate (ptr void) (from_voidp void null) in
  let exitflag = osqp_setup solver_ptr p q_ptr a l_ptr u_ptr m n settings in
  assert (exitflag = 0L);
  Printf.printf "setup: %Ld\n%!" exitflag;

  Printf.printf "before solve\n%!";
  let solver = !@ solver_ptr in
  let exitflag = osqp_solve solver in
  Printf.printf "solve: %Ld\n%!" exitflag;
  assert (exitflag = 0L);

  Printf.printf "before get_solution\n%!";
  let solution = make osqp_solution in
  let x_arr = CArray.make double 2 in  (* n = 2 variables *)
  let y_arr = CArray.make double 3 in  (* m = 3 constraints *)
  let pic_arr = CArray.make double 2 in
  let dic_arr = CArray.make double 3 in
  setf solution x (CArray.start x_arr);
  setf solution y (CArray.start y_arr);
  setf solution prim_inf_cert (CArray.start pic_arr);
  setf solution dual_inf_cert (CArray.start dic_arr);
  let _ = osqp_get_solution solver (addr solution) in
  let x0 = CArray.get x_arr 0 in
  let x1 = CArray.get x_arr 1 in
  Printf.printf "x = [%f, %f]\n" x0 x1;
  ignore (x_arr, y_arr, pic_arr, dic_arr);

  osqp_cleanup solver;
  osqp_csc_matrix_free p;
  osqp_csc_matrix_free a;
  osqp_settings_free settings;

  ignore (p_x, p_i, p_p, a_x, a_i, a_p);

  Printf.printf "PASS\n"
