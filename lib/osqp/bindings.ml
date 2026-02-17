open Ctypes
open Foreign

let osqp_int = int64_t
let osqp_float = double

type osqp_csc_matrix

let osqp_csc_matrix : osqp_csc_matrix structure typ = structure "OSQPCscMatrix"

module Csc_matrix = struct
  let m = field osqp_csc_matrix "m" osqp_int
  let n = field osqp_csc_matrix "n" osqp_int
  let p_ptr = field osqp_csc_matrix "p" (ptr osqp_int)
  let l_ptr = field osqp_csc_matrix "l" (ptr osqp_int)
  let x_ptr = field osqp_csc_matrix "x" (ptr osqp_float)
  let nzmax = field osqp_csc_matrix "nzmax" osqp_int
  let nz = field osqp_csc_matrix "nz" osqp_int
  let owned = field osqp_csc_matrix "owned" osqp_int
end

let () = seal osqp_csc_matrix

type osqp_solution

let osqp_solution : osqp_solution structure typ = structure "OSQPSolution"

module Solution = struct
  let x = field osqp_solution "x" (ptr osqp_float)
  let y = field osqp_solution "y" (ptr osqp_float)
  let prim_inf_cert = field osqp_solution "prim_inf_cert" (ptr double)
  let dual_inf_cert = field osqp_solution "dual_inf_cert" (ptr double)
end

let () = seal osqp_solution

type osqp_settings

let osqp_settings : osqp_settings structure typ = structure "OSQPSettings"

module Settings = struct
  let device = field osqp_settings "device" osqp_int
  let linsys_solver = field osqp_settings "linsys_solver" int
  let allocate_solution = field osqp_settings "allocate_solution" osqp_int
  let verbose = field osqp_settings "verbose" osqp_int
  let profiler_level = field osqp_settings "profiler_level" osqp_int
  let warm_starting = field osqp_settings "warm_starting" osqp_int
  let scaling = field osqp_settings "scaling" osqp_int
  let polishing = field osqp_settings "polishing" osqp_int
  let rho = field osqp_settings "rho" osqp_float
  let rho_is_vec = field osqp_settings "rho_is_vec" osqp_int
  let sigma = field osqp_settings "sigma" osqp_float
  let alpha = field osqp_settings "alpha" osqp_float
  let cg_max_iter = field osqp_settings "cg_max_iter" osqp_int
  let cg_tol_reduction = field osqp_settings "cg_tol_reduction" osqp_int
  let cg_tol_fraction = field osqp_settings "cg_tol_fraction" osqp_float
  let cg_precond = field osqp_settings "cg_precond" int
  let adaptive_rho = field osqp_settings "adaptive_rho" osqp_int

  let adaptive_rho_interval =
    field osqp_settings "adaptive_rho_interval" osqp_int

  let adaptive_rho_fraction =
    field osqp_settings "adaptive_rho_fraction" osqp_float

  let adaptive_rho_tolerance =
    field osqp_settings "adaptive_rho_tolerance" osqp_float

  let max_iter = field osqp_settings "max_iter" osqp_int
  let eps_abs = field osqp_settings "eps_abs" osqp_float
  let eps_rel = field osqp_settings "eps_rel" osqp_float
  let eps_prim_inf = field osqp_settings "eps_prim_inf" osqp_float
  let eps_dual_inf = field osqp_settings "eps_dual_inf" osqp_float
  let scaled_termination = field osqp_settings "scaled_termination" osqp_int
  let check_termination = field osqp_settings "check_termination" osqp_int
  let check_dualgap = field osqp_settings "check_dualgap" osqp_int
  let time_limit = field osqp_settings "time_limit" osqp_float
  let delta = field osqp_settings "delta" osqp_float
  let polish_refine_iter = field osqp_settings "polish_refine_iter" osqp_int
end

let () = seal osqp_settings

type osqp_info

let osqp_info : osqp_info structure typ = structure "OSQPInfo"

module Info = struct
  let status = field osqp_info "status" (array 32 char)
  let status_val = field osqp_info "status_val" osqp_int
  let status_polish = field osqp_info "status_polish" osqp_int
  let obj_val = field osqp_info "obj_val" osqp_float
  let dual_obj_val = field osqp_info "dual_obj_val" osqp_float
  let prim_res = field osqp_info "prim_res" osqp_float
  let dual_res = field osqp_info "dual_res" osqp_float
  let duality_gap = field osqp_info "duality_gap" osqp_float
  let iter = field osqp_info "iter" osqp_int
  let rho_updates = field osqp_info "rho_updates" osqp_int
  let rho_estimate = field osqp_info "rho_estimate" osqp_float
  let setup_time = field osqp_info "setup_time" osqp_float
  let solve_time = field osqp_info "solve_time" osqp_float
  let update_time = field osqp_info "update_time" osqp_float
  let polish_time = field osqp_info "polish_time" osqp_float
  let run_time = field osqp_info "run_time" osqp_float
  let primdual_int = field osqp_info "primdual_int" osqp_float
  let rel_kkt_error = field osqp_info "rel_kkt_error" osqp_float
end

let () = seal osqp_info

type osqp_solver

let osqp_solver : osqp_solver structure typ = structure "OSQPSolver"

module Solver = struct
  let settings = field osqp_solver "settings" (ptr void)
  let solution = field osqp_solver "solution" (ptr osqp_solution)
  let info = field osqp_solver "info" (ptr osqp_info)
  let work = field osqp_solver "work" (ptr void)
end

let () = seal osqp_solver

let osqp_csc_matrix_new =
  foreign "OSQPCscMatrix_new"
    (osqp_int @-> osqp_int @-> osqp_int @-> ptr osqp_float @-> ptr osqp_int
   @-> ptr osqp_int
    @-> returning (ptr osqp_csc_matrix))

let osqp_csc_matrix_free =
  foreign "OSQPCscMatrix_free" (ptr osqp_csc_matrix @-> returning void)

let osqp_solve = foreign "osqp_solve" (ptr osqp_solver @-> returning osqp_int)

let osqp_setup =
  foreign "osqp_setup"
    (ptr (ptr osqp_solver)
    @-> ptr osqp_csc_matrix @-> ptr osqp_float @-> ptr osqp_csc_matrix
    @-> ptr osqp_float @-> ptr osqp_float @-> osqp_int @-> osqp_int
    @-> ptr osqp_settings @-> returning osqp_int)

let osqp_cleanup = foreign "osqp_cleanup" (ptr osqp_solver @-> returning void)

let osqp_settings_new =
  foreign "OSQPSettings_new" (void @-> returning (ptr osqp_settings))

let osqp_settings_free =
  foreign "OSQPSettings_free" (ptr osqp_settings @-> returning void)

let osqp_set_default_settings =
  foreign "osqp_set_default_settings" (ptr osqp_settings @-> returning void)

let osqp_get_solution =
  foreign "osqp_get_solution"
    (ptr osqp_solver @-> ptr osqp_solution @-> returning osqp_int)

let osqp_update_data_vec =
  foreign "osqp_update_data_vec"
    (ptr osqp_solver @-> ptr_opt osqp_float @-> ptr_opt osqp_float
   @-> ptr_opt osqp_float @-> returning osqp_int)

let osqp_update_data_mat =
  foreign "osqp_update_data_mat"
    (ptr osqp_solver @-> ptr_opt osqp_float @-> ptr_opt osqp_int @-> osqp_int
   @-> ptr_opt osqp_float @-> ptr_opt osqp_int @-> osqp_int
   @-> returning osqp_int)

let osqp_warm_start =
  foreign "osqp_warm_start"
    (ptr osqp_solver @-> ptr_opt osqp_float @-> ptr_opt osqp_float
   @-> returning osqp_int)
