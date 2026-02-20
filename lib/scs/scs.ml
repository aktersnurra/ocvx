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

type t = { scs_work : Bindings.scs_work structure ptr }
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

let empty_cone =
  {
    z = 0;
    l = 0;
    bu = [||];
    bl = [||];
    q = [||];
    s = [||];
    cs = [||];
    ep = 0;
    ed = 0;
    p = [||];
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

let default_settings =
  {
    normalize = true;
    scale = 0.1;
    adaptive_scale = true;
    rho_x = 1e-6;
    max_iters = 100000;
    eps_abs = 1e-4;
    eps_rel = 1e-4;
    eps_infeas = 1e-7;
    alpha = 1.5;
    time_limit_secs = 0.0;
    verbose = true;
    warm_start = false;
    acceleration_lookback = 10;
    acceleration_interval = 10;
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
  (mat, x, i, p)

let to_scs_data ~a ~p ~b ~c =
  let m = Array.length a in
  let n = Array.length a.(0) in 
  let* p = check_symmetric p |> upper_triangular |> csc_of_dense |> scs_of_csc in
  let* a = csc_of_dense a |> scs_of_csc in
  let b, b_ptr = floats b in
  let c, c_ptr = floats c in
  let data = make Bindings.scs_data in
  setf data Bindings.Data.m m;
  setf data Bindings.Data.n n;
  setf data Bindings.Data.a a;
  setf data Bindings.Data.p p;
  setf data Bindings.Data.b b;
  setf data Bindings.Data.c c;
  data

let configure_settings settings =
  let s = make Bindings.scs_settings in
  Bindings.scs_set_default_settings (addr s);
  setf s Bindings.Settings.normalize (if settings.normalize then 1 else 0);
  setf s Bindings.Settings.scale settings.scale;
  setf s Bindings.Settings.adaptive_scale
    (if settings.adaptive_scale then 1 else 0);
  setf s Bindings.Settings.rho_x settings.rho_x;
  setf s Bindings.Settings.max_iters settings.max_iters;
  setf s Bindings.Settings.eps_abs settings.eps_abs;
  setf s Bindings.Settings.eps_rel settings.eps_rel;
  setf s Bindings.Settings.eps_infeas settings.eps_infeas;
  setf s Bindings.Settings.alpha settings.alpha;
  setf s Bindings.Settings.time_limit_secs settings.time_limit_secs;
  setf s Bindings.Settings.verbose (if settings.verbose then 1 else 0);
  setf s Bindings.Settings.warm_start (if settings.warm_start then 1 else 0);
  setf s Bindings.Settings.acceleration_lookback settings.acceleration_lookback;
  setf s Bindings.Settings.acceleration_interval settings.acceleration_interval;
  s

let array_opt ~f arr =
  if Array.length arr = 0 then (None, None)
  else let ca, ptr = f arr in (Some ca, Some ptr)

let configure_cone cone =
  let k = make Bindings.scs_cone in
  setf k Bindings.Cone.z cone.z;
  setf k Bindings.Cone.l cone.l;
  setf k Bindings.Cone.ep cone.ep;
  setf k Bindings.Cone.ed cone.ed;
  let bu, bu_ptr = array_opt ~f:floats cone.bu in
  let bl, bl_ptr = array_opt ~f:floats cone.bl in
  setf k Bindings.Cone.bu bu_ptr;
  setf k Bindings.Cone.bl bl_ptr;
  setf k Bindings.Cone.bsize (Array.length cone.bu + 1);
  let q, q_ptr = array_opt ~f:ints cone.q in
  setf k Bindings.Cone.q q_ptr;
  setf k Bindings.Cone.qsize (Array.length cone.q);
  let s, s_ptr = array_opt ~f:ints cone.s in
  setf k Bindings.Cone.s s_ptr;
  setf k Bindings.Cone.ssize (Array.length cone.s);
  let cs, cs_ptr = array_opt ~f:ints cone.cs in
  setf k Bindings.Cone.cs cs_ptr;
  setf k Bindings.Cone.cssize (Array.length cone.cs);
  let p, p_ptr = array_opt ~f:floats cone.p in
  setf k Bindings.Cone.p p_ptr;
  setf k Bindings.Cone.psize (Array.length cone.p);
  ignore (bu, bl, q, s, cs, p);
  k

(* * @param  d      Problem data. *)
(* * @param  k      Cone data. *)
(* * @param  stgs   SCS solve settings. *)
(* ScsWork *scs_init(const ScsData *d, const ScsCone *k, const ScsSettings *stgs); *)
let setup ?(settings = default_settings) ~c ~a ~b ?p cone () =
  let d = to_scs_data ~m ~n ~a ~p ~b:b_ptr ~c:c_ptr in 
  let k = configure_cone cone in
  let stgs = configure_settings settings in
  let scs_work = Bindings.scs_init d k stgs in
  { scs_work }

(* let solve warm_start t = *)
(**)
(**)
(* let update : *)
(*   t -> ?b:float array -> ?c:float array -> unit -> (unit, error) result *)
