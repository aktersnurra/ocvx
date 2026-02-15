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
