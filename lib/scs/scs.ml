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

type t = { scs_work : Bindings.scs_work structure ptr; m : int; n : int }
type solution = { x : float array; y : float array; s : float array }

type info = {
  iter : int;
  status : status;
  pobj : float;
  dobj : float;
  setup_time : float;
  solve_time : float;
}

type error = Init_failed | Solve_failed of status | Invalid_data of string

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

let floats arr = to_carray ~t:Bindings.scs_float ~f:Fun.id arr
let ints arr = to_carray ~t:Bindings.scs_int ~f:Fun.id arr

let carray_opt ~f arr =
  if Array.length arr = 0 then (None, None)
  else
    let ca, ptr = f arr in
    (Some ca, Some ptr)

let floats_opt = carray_opt ~f:floats
let ints_opt = carray_opt ~f:ints

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
  (mat, (x, i, p))

let to_scs_data ~a ?p ~b ~c () =
  let prepare_p = function
    | None -> Ok (None, None)
    | Some p ->
        check_symmetric p
        |> Result.map_error (fun e -> Invalid_data e)
        |> Result.map (fun p ->
            let mat, _p = upper_triangular p |> csc_of_dense |> scs_of_csc in
            (Some mat, Some _p))
  in
  let m = Array.length a in
  let n = Array.length a.(0) in
  let a_mat, _a = csc_of_dense a |> scs_of_csc in
  let* p_mat, _p = prepare_p p in
  let _b, b_ptr = floats b in
  let _c, c_ptr = floats c in
  let data = make Bindings.scs_data in
  setf data Bindings.Data.m m;
  setf data Bindings.Data.n n;
  setf data Bindings.Data.a (addr a_mat);
  setf data Bindings.Data.p (Option.map addr p_mat);
  setf data Bindings.Data.b b_ptr;
  setf data Bindings.Data.c c_ptr;
  ignore (_a, _p, _b, _c);
  Ok (data, m, n)

let configure_settings settings =
  let scs_int_of_bool b = if b then 1 else 0 in
  let s = make Bindings.scs_settings in
  Bindings.scs_set_default_settings (addr s);
  setf s Bindings.Settings.normalize (scs_int_of_bool settings.normalize);
  setf s Bindings.Settings.scale settings.scale;
  setf s Bindings.Settings.adaptive_scale
    (scs_int_of_bool settings.adaptive_scale);
  setf s Bindings.Settings.rho_x settings.rho_x;
  setf s Bindings.Settings.max_iters settings.max_iters;
  setf s Bindings.Settings.eps_abs settings.eps_abs;
  setf s Bindings.Settings.eps_rel settings.eps_rel;
  setf s Bindings.Settings.eps_infeas settings.eps_infeas;
  setf s Bindings.Settings.alpha settings.alpha;
  setf s Bindings.Settings.time_limit_secs settings.time_limit_secs;
  setf s Bindings.Settings.verbose (scs_int_of_bool settings.verbose);
  setf s Bindings.Settings.warm_start (scs_int_of_bool settings.warm_start);
  setf s Bindings.Settings.acceleration_lookback settings.acceleration_lookback;
  setf s Bindings.Settings.acceleration_interval settings.acceleration_interval;
  s

let configure_cone cone =
  let k = make Bindings.scs_cone in
  setf k Bindings.Cone.z cone.z;
  setf k Bindings.Cone.l cone.l;
  setf k Bindings.Cone.ep cone.ep;
  setf k Bindings.Cone.ed cone.ed;
  let _bu, bu_ptr = floats_opt cone.bu in
  let _bl, bl_ptr = floats_opt cone.bl in
  setf k Bindings.Cone.bu bu_ptr;
  setf k Bindings.Cone.bl bl_ptr;
  setf k Bindings.Cone.bsize (Array.length cone.bu + 1);
  let _q, q_ptr = ints_opt cone.q in
  setf k Bindings.Cone.q q_ptr;
  setf k Bindings.Cone.qsize (Array.length cone.q);
  let _s, s_ptr = ints_opt cone.s in
  setf k Bindings.Cone.s s_ptr;
  setf k Bindings.Cone.ssize (Array.length cone.s);
  let _cs, cs_ptr = ints_opt cone.cs in
  setf k Bindings.Cone.cs cs_ptr;
  setf k Bindings.Cone.cssize (Array.length cone.cs);
  let _p, p_ptr = floats_opt cone.p in
  setf k Bindings.Cone.p p_ptr;
  setf k Bindings.Cone.psize (Array.length cone.p);
  ignore (_bu, _bl, _q, _s, _cs, _p);
  k

let cleanup t = Bindings.scs_finish t.work

let setup ?(settings = default_settings) ~c ~a ~b ?p ~cone () =
  let* d, m, n = to_scs_data ~a ?p ~b ~c () in
  let k = configure_cone cone in
  let stgs = configure_settings settings in
  let work = Bindings.scs_init (addr d) (addr k) (addr stgs) in
  if is_null work then Error Init_failed
  else
    let t = { work; n; m } in
    Gc.finalise (fun t -> Bindings.scs_finish t.work) t;
    Ok t

let solve warm_start t =
  let solution = make Bindings.scs_solution in
  setf solution Bindings.Solution.x (from_voidp Bindings.scs_float null);
  setf solution Bindings.Solution.y (from_voidp Bindings.scs_float null);
  setf solution Bindings.Solution.s (from_voidp Bindings.scs_float null);
  let info = make Bindings.scs_info in
  let exitflag = Bindings.scs_solve t.work (addr solution) (addr info) in
  if exitflag = 0 then Ok ()
  else exitflag |> status_of_int |> fun s -> Error (Solve_failed s)



(* let update : *)
(*   t -> ?b:float array -> ?c:float array -> unit -> (unit, error) result *)
