open Ctypes
open Foreign
open Util

type status =
  | Failed
  | Indeterminate
  | Infeasible
  | Infeasible_inaccurate
  | Max_iter_reached
  | Sigint
  | Solved
  | Solved_inaccurate
  | Unbounded
  | Unbounded_inaccurate
  | Unfinished
  | Unknown of int


let status_of_int = function
  | -1 -> Unbounded
  | -2 -> Infeasible
  | -3 -> Indeterminate
  | -4 -> Failed
  | -5 -> Sigint
  | -6 -> Unbounded_inaccurate
  | -7 -> Infeasible_inaccurate
  | 0 -> Unfinished
  | 1 -> Solved
  | 2 -> Solved_inaccurate
  | n -> Unknown n


type t = {
  scs_work: Bindings.scs_work structure ptr;
}


type solution = { x : float array; y : float array; s : float array }


type info = {
  iter : int;
  status : status;
  pobj : float;
  dobj : float;
  setup_time : float;
  solve_time : float;
}

type error = Init_failed | Solve_failed of status
type cone = { z : int; l : int; q : int array; ep : int }

type settings = {
  max_iters : int;
  eps_abs : float;
  eps_rel : float;
  verbose : bool;
}


let default_settings = {
  max_iters = 100000;
  eps_abs = 1e-4;
  eps_rel = 1e-4;
  verbose = true;
}

let floats arr = to_carray ~t:Bindings.scs_float ~f:Fun.id
let ints arr = to_carray ~t:Bindings.scs_int ~f:Fun.id

let scs_of_csc csc = 
  let x, x_ptr = floats csc.values in
  let i, i_ptr = ints csc.row_indices in
  let p, p_ptr = ints csc.col_pointers in
  let mat = make Bindings.scs_matrix in
  setf mat Bindings.Matrix.x x_ptr;
  setf mat Bindings.Matrix.i i_ptr;
  setf mat Bindings.Matrix.p p_ptr;
  setf mat Bindings.Matrix.m csc.nrows;
  setf mat Bindings.Matrix.n csc.ncols;
  mat


(* let data  *)



 (* * @param  d      Problem data. *)
 (* * @param  k      Cone data. *)
 (* * @param  stgs   SCS solve settings. *)
(* ScsWork *scs_init(const ScsData *d, const ScsCone *k, const ScsSettings *stgs); *)
let setup ?(settings = default_settings) ~c ~a ~b ?p cone () =
  
  

(* let solve warm_start t = *)
(**)
(**)
(* let update : *)
(*   t -> ?b:float array -> ?c:float array -> unit -> (unit, error) result *)
