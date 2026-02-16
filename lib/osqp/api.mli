type csc_matix

type settings = {
  alpha : float;
  verbose: bool;
  max_iter: int;
  eps_abs : float;
  eps_rel: float;
  polish: bool;
}

let default = {
  alpha = 1.6;
  verbose = true;
  max_iter = 4_000;
  eps_abs = 1e-3;
  eps_rel = 1e-3;
  polish = false;
}

type t

val setup : settings -> p:float array -> q:float array -> a:float array -> l:float array -> u:float array -> t
val solve : t -> () result
val warm_start: t -> x:float array -> y:float array -> () result
(* val update_data_vec: t -> q:float array -> l:float array -> u:float array -> () result *)
(* val update_data_mat: t -> q:float array -> l:float array -> u:float array -> () result *)
