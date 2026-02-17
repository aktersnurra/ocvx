type t
type solution = { x : float array; y : float array; s : float array }

type status =
  | Solved
  | Solved_inaccurate
  | Infeasible
  | Unbounded
  | Max_iter_reached
  | Failed
  | Unknown of int

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

val default_settings : settings

val setup :
  ?settings:settings ->
  c:float array ->
  a:float array array ->
  b:float array ->
  ?p:float array array ->
  cone:cone ->
  unit ->
  (t, error) result

val solve : ?warm_start:bool -> t -> (solution * info, error) result

val update :
  t -> ?b:float array -> ?c:float array -> unit -> (unit, error) result
