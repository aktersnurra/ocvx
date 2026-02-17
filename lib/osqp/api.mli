type status =
  | Solved
  | Solved_inaccurate
  | Primal_infeasible
  | Primal_infeasible_inaccurate
  | Dual_infeasible
  | Dual_infeasible_inaccurate
  | Max_iter_reached
  | Time_limit_reached
  | Non_convex
  | Sigint
  | Unsolved
  | Unknown of int

type settings = {
  alpha : float;
  verbose : bool;
  max_iter : int;
  eps_abs : float;
  eps_rel : float;
  polish : bool;
}

type solution = { x : float array; y : float array }

val default_settings : settings

type error =
  | Setup_failed of int
  | Solve_failed of status
  | Setting_failed of int
  | Solution_failed of int

type t

val setup :
  ?settings:settings ->
  p:float array array ->
  q:float array ->
  a:float array array ->
  l:float array ->
  u:float array ->
  unit ->
  (t, error) result

val solve : t -> (solution, error) result

val warm_start :
  t -> ?x:float array -> ?y:float array -> unit -> (unit, error) result

val update_vectors :
  t ->
  ?q:float array ->
  ?l:float array ->
  ?u:float array ->
  unit ->
  (unit, error) result

val update_matrices :
  t ->
  ?p:float array array ->
  ?a:float array array ->
  unit ->
  (unit, error) result
