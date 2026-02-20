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
  device : int;
  verbose : bool;
  warm_starting : bool;
  scaling : int;
  polishing : bool;
  rho : float;
  rho_is_vec : bool;
  sigma : float;
  alpha : float;
  cg_max_iter : int;
  cg_tol_reduction : int;
  cg_tol_fraction : float;
  adaptive_rho : int;
  adaptive_rho_interval : int;
  adaptive_rho_fraction : float;
  adaptive_rho_tolerance : float;
  max_iter : int;
  eps_abs : float;
  eps_rel : float;
  eps_prim_inf : float;
  eps_dual_inf : float;
  scaled_termination : bool;
  check_termination : int;
  check_dualgap : bool;
  time_limit : float;
  delta : float;
  polish_refine_iter : int;
}

type solution = { x : float array; y : float array }

val default_settings : settings

type error =
  | Setup_failed of int
  | Solve_failed of status
  | Setting_failed of int
  | Solution_failed of int
  | Warm_start_failed of int
  | Update_vectors_failed of int
  | Update_matrices_failed of int

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
