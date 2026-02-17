open Ctypes
open Foreign

let scs_int = int
let scs_float = double

type scs_work
let scs_work : scs_work structure typ = structure "SCS_WORK"
let () = seal scs_work

type scs_matrix
let scs_matrix : scs_matrix structure typ = structure "ScsMatrix"
module Matrix = struct
  let x = field scs_matrix "x" (ptr scs_float)
  let i = field scs_matrix "i" (ptr scs_int)
  let p = field scs_matrix "p" (ptr scs_int)
  let m = field scs_matrix "m" scs_int
  let n = field scs_matrix "n" scs_int
  let () = seal scs_matrix
end

type scs_settings
let scs_settings : scs_settings structure typ = structure "ScsSettings"
module Settings = struct
  let normalize = field scs_settings "normalize" scs_int
  let scale = field scs_settings "scale" scs_float
  let adaptive_scale = field scs_settings "adaptive_scale" scs_int
  let rho_x = field scs_settings "rho_x" scs_float
  let max_iters = field scs_settings "max_iters" scs_int
  let eps_abs = field scs_settings "eps_abs" scs_float
  let eps_rel = field scs_settings "eps_rel" scs_float
  let eps_infeas = field scs_settings "eps_infeas" scs_float
  let alpha = field scs_settings "alpha" scs_float
  let time_limit_secs = field scs_settings "time_limit_secs" scs_float
  let verbose = field scs_settings "verbose" scs_int
  let warm_start = field scs_settings "warm_start" scs_int
  let acceleration_lookback = field scs_settings "acceleration_lookback" scs_int
  let acceleration_interval = field scs_settings "acceleration_interval" scs_int
  let write_data_filename = field scs_settings "write_data_filename" (ptr_opt char)
  let log_csv_filename = field scs_settings "log_csv_filename" (ptr_opt char)
  let () = seal scs_settings
end

type scs_data
let scs_data : scs_data structure typ = structure "ScsData"
module Data = struct
  let m = field scs_data "m" scs_int
  let n = field scs_data "n" scs_int
  let a = field scs_data "A" (ptr scs_matrix)
  let p = field scs_data "P" (ptr_opt scs_matrix)
  let b = field scs_data "b" (ptr scs_float)
  let c = field scs_data "c" (ptr scs_float)
  let () = seal scs_data
end

type scs_cone
let scs_cone : scs_cone structure typ = structure "ScsCone"
module Cone = struct
  let z = field scs_cone "z" scs_int
  let l = field scs_cone "l" scs_int
  let bu = field scs_cone "bu" (ptr_opt scs_float)
  let bl = field scs_cone "bl" (ptr_opt scs_float)
  let bsize = field scs_cone "bsize" scs_int
  let q = field scs_cone "q" (ptr_opt scs_int)
  let qsize = field scs_cone "qsize" scs_int
  let s = field scs_cone "s" (ptr_opt scs_int)
  let ssize = field scs_cone "ssize" scs_int
  let cs = field scs_cone "cs" (ptr_opt scs_int)
  let cssize = field scs_cone "cssize" scs_int
  let ep = field scs_cone "ep" scs_int
  let ed = field scs_cone "ed" scs_int
  let p = field scs_cone "p" (ptr_opt scs_float)
  let psize = field scs_cone "psize" scs_int
  let () = seal scs_cone
end

type scs_solution
let scs_solution : scs_solution structure typ = structure "ScsSolution"
module Solution = struct
  let x = field scs_solution "x" (ptr scs_float)
  let y = field scs_solution "y" (ptr scs_float)
  let s = field scs_solution "s" (ptr scs_float)
  let () = seal scs_solution
end

type scs_info
let scs_info : scs_info structure typ = structure "ScsInfo"
module Info = struct
  let iter = field scs_info "iter" scs_int
  let status = field scs_info "status" (array 128 char)
  let lin_sys_solver = field scs_info "lin_sys_solver" (array 128 char)
  let status_val = field scs_info "status_val" scs_int
  let scale_updates = field scs_info "scale_updates" scs_int
  let pobj = field scs_info "pobj" scs_float
  let dobj = field scs_info "dobj" scs_float
  let res_pri = field scs_info "res_pri" scs_float
  let res_dual = field scs_info "res_dual" scs_float
  let gap = field scs_info "gap" scs_float
  let res_infeas = field scs_info "res_infeas" scs_float
  let res_unbdd_a = field scs_info "res_unbdd_a" scs_float
  let res_unbdd_p = field scs_info "res_unbdd_p" scs_float
  let setup_time = field scs_info "setup_time" scs_float
  let solve_time = field scs_info "solve_time" scs_float
  let scale = field scs_info "scale" scs_float
  let comp_slack = field scs_info "comp_slack" scs_float
  let rejected_accel_steps = field scs_info "rejected_accel_steps" scs_int
  let accepted_accel_steps = field scs_info "accepted_accel_steps" scs_int
  let lin_sys_time = field scs_info "lin_sys_time" scs_float
  let cone_time = field scs_info "cone_time" scs_float
  let accel_time = field scs_info "accel_time" scs_float
  let () = seal scs_info
end

let scs_init = foreign "scs_init"
  (ptr scs_data @-> ptr scs_cone @-> ptr scs_settings @-> returning (ptr scs_work))

let scs_solve = foreign "scs_solve"
  (ptr scs_work @-> ptr scs_solution @-> ptr scs_info @-> scs_int @-> returning scs_int)

let scs_update = foreign "scs_update"
  (ptr scs_work @-> ptr_opt scs_float @-> ptr_opt scs_float @-> returning scs_int)

let scs_finish = foreign "scs_finish"
  (ptr scs_work @-> returning void)

let scs_set_default_settings = foreign "scs_set_default_settings"
  (ptr scs_settings @-> returning void)

let scs_version = foreign "scs_version"
  (void @-> returning string)
