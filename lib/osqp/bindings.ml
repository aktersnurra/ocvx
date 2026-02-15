open Ctypes
open Foreign

let osqp_int = int64_t
let osqp_float = double

type osqp_csc_matrix
let osqp_csc_matrix : osqp_csc_matrix structure typ = structure "OSQPCscMatrix"
let m = field osqp_csc_matrix "m" osqp_int
let n = field osqp_csc_matrix "n" osqp_int
let p_ptr = field osqp_csc_matrix "p" (ptr osqp_int)
let l_ptr = field osqp_csc_matrix "l" (ptr osqp_int)
let x_ptr = field osqp_csc_matrix "x" (ptr osqp_float)
let nzmax = field osqp_csc_matrix "nzmax" osqp_int
let nz = field osqp_csc_matrix "nz" osqp_int
let owned = field osqp_csc_matrix "owned" osqp_int
let () = seal osqp_csc_matrix

type osqp_settings
let osqp_settings : osqp_settings structure typ = structure "OSQPSettings"
let () = seal osqp_settings

type osqp_solution
let osqp_solution : osqp_solution structure typ = structure "OSQPSolution"
let x = field osqp_solution "x" (ptr osqp_float)
let y = field osqp_solution "y" (ptr osqp_float)
let prim_inf_cert = field osqp_solution "prim_inf_cert" (ptr double)
let dual_inf_cert = field osqp_solution "dual_inf_cert" (ptr double)
let () = seal osqp_solution

(* typedef struct { *)
(*   // solver status *)
(*   char    status[32];     ///< Status string, e.g. 'solved' *)
(*   OSQPInt status_val;     ///< Status as OSQPInt, defined in osqp_api_constants.h *)
(*   OSQPInt status_polish;  ///< Polishing status: successful (1), unperformed (0), unsuccessful (-1) *)
(**)
(*   // solution quality *)
(*   OSQPFloat obj_val;      ///< Primal objective value *)
(*   OSQPFloat dual_obj_val; ///< Dual objective value *)
(*   OSQPFloat prim_res;     ///< Norm of primal residual *)
(*   OSQPFloat dual_res;     ///< Norm of dual residual *)
(*   OSQPFloat duality_gap;  ///< Duality gap (Primal obj - Dual obj) *)
(**)
(*   // algorithm information *)
(*   OSQPInt   iter;         ///< Number of iterations taken *)
(*   OSQPInt   rho_updates;  ///< Number of rho updates performned *)
(*   OSQPFloat rho_estimate; ///< Best rho estimate so far from residuals *)
(**)
(*   // timing information *)
(*   OSQPFloat setup_time;  ///< Setup phase time (seconds) *)
(*   OSQPFloat solve_time;  ///< Solve phase time (seconds) *)
(*   OSQPFloat update_time; ///< Update phase time (seconds) *)
(*   OSQPFloat polish_time; ///< Polish phase time (seconds) *)
(*   OSQPFloat run_time;    ///< Total solve time (seconds) *)
(**)
(*   // Convergence information *)
(*   OSQPFloat primdual_int;  ///< Integral of duality gap over time (Primal-dual integral), requires profiling *)
(*   OSQPFloat rel_kkt_error; ///< Relative KKT error *)
(* } OSQPInfo; *)

(* typedef struct { *)
(*   /** @} */ *)
(*   OSQPSettings*  settings; ///< Problem settings *)
(*   OSQPSolution*  solution; ///< Computed solution *)
(*   OSQPInfo*      info;     ///< Solver information *)
(*   OSQPWorkspace* work;     ///< Internal solver workspace (contents not public) *)
(* } OSQPSolver; *)
type osqp_solve

let floats arr =
  let n = Array.length arr in
  let ca = CArray.make osqp_float n in
  Array.iteri (fun i v -> CArray.set ca i v) arr;
  (ca, CArray.start ca)

let ints arr =
  let n = Array.length arr in
  let ca = CArray.make osqp_int n in
  Array.iteri (fun i v -> CArray.set ca i v) arr;
  (ca, CArray.start ca)

let osqp_csc_matrix_new =
  foreign "OSQPCscMatrix_new"
    (int64_t @-> int64_t @-> int64_t @-> ptr double @-> ptr int64_t
   @-> ptr int64_t
    @-> returning (ptr void))

let osqp_csc_matrix_free =
  foreign "OSQPCscMatrix_free" (ptr void @-> returning void)

let osqp_solve = foreign "osqp_solve" (ptr void @-> returning osqp_int)

let osqp_setup =
  foreign "osqp_setup"
    (ptr (ptr void)
    @-> ptr void @-> ptr osqp_float @-> ptr void @-> ptr osqp_float
    @-> ptr osqp_float @-> osqp_int @-> osqp_int @-> ptr void
    @-> returning osqp_int)

let osqp_cleanup = foreign "osqp_cleanup" (ptr void @-> returning void)

let osqp_settings_new =
  foreign "OSQPSettings_new" (void @-> returning (ptr void))

let osqp_settings_free =
  foreign "OSQPSettings_free" (ptr void @-> returning void)

let osqp_set_default_settings =
  foreign "osqp_set_default_settings" (ptr void @-> returning void)

let osqp_get_solution =
  foreign "osqp_get_solution"
    (ptr void @-> ptr osqp_solution @-> returning osqp_int)
