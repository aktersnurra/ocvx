type t
type solution = { x : float array; y : float array; s : float array }

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

type info = {
  iter : int;
  status : status;
  pobj : float;
  dobj : float;
  setup_time : float;
  solve_time : float;
}

type error =
  | Init_failed
  | Solve_failed of status
  | Invalid_data of string
  | Update_failed of int

type cone = {
  z : int;
  l : int;
  bu : float array;
  bl : float array;
  q : int array;
  s : int array;
  cs : int array;
  ep : int;
  ed : int;
  p : float array;
}

type settings = {
  normalize : bool;
  scale : float;
  adaptive_scale : bool;
  rho_x : float;
  max_iters : int;
  eps_abs : float;
  eps_rel : float;
  eps_infeas : float;
  alpha : float;
  time_limit_secs : float;
  verbose : bool;
  warm_start : bool;
  acceleration_lookback : int;
  acceleration_interval : int;
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

val solve :
  ?warm_start:bool ->
  ?prev_sol:solution ->
  t ->
  unit ->
  (solution * info, error) result

val update :
  t -> ?b:float array -> ?c:float array -> unit -> (unit, error) result
