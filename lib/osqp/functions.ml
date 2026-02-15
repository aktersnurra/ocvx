open Ctypes
open Foreign

let osqp_int = int64_t
let osqp_float = double

let floats l =
  let ca = CArray.of_list osqp_float l in
  ca, CArray.start ca

let ints l =
  let ca = CArray.of_list osqp_int l in
  ca, CArray.start ca


type osqp_solver = unit ptr
let osqp_solver : osqp_solver typ = ptr void

type osqp_settings = unit ptr
let osqp_settings : osqp_settings typ = ptr void

type osqp_csc = unit ptr
let osqp_csc : osqp_csc typ = unit ptr

let osqp_csc_matrix_new = foreign "OSQPCscMatrix_new"
  (int64_t @-> int64_t @-> int64_t @->
   ptr double @-> ptr int64_t @-> ptr int64_t @->
   returning (ptr void))

let osqp_csc_matrix_free = foreign "OSQPCscMatrix_free" (ptr void @-> returning void)

let osqp_solve = foreign "osqp_solve" (ptr void @ -> returning osqp_float)

let osqp_setup = foreign "osqp_setup"
  (ptr osqp_solver @->
  ptr osqp_csc @->
  ptr osqp_double @->
  ptr osqp_csc @->
  ptr osqp_double @->
  ptr osqp_double @->
  ptr osqp_int @->
  ptr osqp_int @->
  ptr osqp_int @->
  ptr osqp_settings @->
  returning osqp_int)

let osqp_cleanup = foreign "osqp_cleanup" (osqp_solver @-> returning void)
