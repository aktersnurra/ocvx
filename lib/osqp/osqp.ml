open Ctypes
open Foreign
open Result.Syntax
open Common

type osqp_csc_matix = {
  ptr : Bindings.osqp_csc_matrix structure ptr;
  _x : float CArray.t;
  _i : int64 CArray.t;
  _p : int64 CArray.t;
}

type qp = {
  m : int;
  n : int;
  p : osqp_csc_matix;
  a : osqp_csc_matix;
  q : float CArray.t;
  l : float CArray.t;
  u : float CArray.t;
}

type t = {
  solver : Bindings.osqp_solver structure ptr;
  qp : qp;
}

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

let default_settings =
  {
    device = 0;
    verbose = true;
    warm_starting = true;
    scaling = 10;
    polishing = false;
    rho = 0.1;
    rho_is_vec = true;
    sigma = 1e-6;
    alpha = 1.6;
    cg_max_iter = 20;
    cg_tol_reduction = 10;
    cg_tol_fraction = 0.15;
    adaptive_rho = 1;
    adaptive_rho_interval = 50;
    adaptive_rho_fraction = 0.4;
    adaptive_rho_tolerance = 5.0;
    max_iter = 4000;
    eps_abs = 1e-3;
    eps_rel = 1e-3;
    eps_prim_inf = 1e-4;
    eps_dual_inf = 1e-4;
    scaled_termination = false;
    check_termination = 25;
    check_dualgap = true;
    time_limit = 1e10;
    delta = 1e-6;
    polish_refine_iter = 3;
  }

type solution = { x : float array; y : float array }

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

type error =
  | Setup_failed of int
  | Solve_failed of status
  | Setting_failed of int
  | Solution_failed of int
  | Warm_start_failed of int
  | Update_vectors_failed of int
  | Update_matrices_failed of int

let status_of_int = function
  | 1 -> Solved
  | 2 -> Solved_inaccurate
  | 3 -> Primal_infeasible
  | 4 -> Primal_infeasible_inaccurate
  | 5 -> Dual_infeasible
  | 6 -> Dual_infeasible_inaccurate
  | 7 -> Max_iter_reached
  | 8 -> Time_limit_reached
  | 9 -> Non_convex
  | 10 -> Sigint
  | 11 -> Unsolved
  | n -> Unknown n

let floats arr = to_carray ~t:Bindings.osqp_float ~f:Fun.id
let ints arr = to_carray ~t:Bindings.osqp_int ~f:Int64.of_int

let osqp_of_csc csc =
  let x, x_ptr = floats csc.values in
  let i, i_ptr = ints csc.row_indices in
  let p, p_ptr = ints csc.col_pointers in
  let ptr =
    Bindings.osqp_csc_matrix_new (Int64.of_int csc.nrows)
      (Int64.of_int csc.ncols)
      (Int64.of_int (nnz csc))
      x_ptr i_ptr p_ptr
  in
  { ptr; _x = x; _i = i; _p = p }

let osqp_int_of_bool b = Int64.of_int (if b then 1 else 0)
let ospq_int_of_int = Int64.of_int

let configure_settings settings =
  let s_ptr = Bindings.osqp_settings_new () in
  Bindings.osqp_set_default_settings s_ptr;
  let s = !@s_ptr in
  setf s Bindings.Settings.device (ospq_int_of_int settings.device);
  setf s Bindings.Settings.verbose (osqp_int_of_bool settings.verbose);
  setf s Bindings.Settings.warm_starting
    (osqp_int_of_bool settings.warm_starting);
  setf s Bindings.Settings.scaling (ospq_int_of_int settings.scaling);
  setf s Bindings.Settings.polishing (osqp_int_of_bool settings.polishing);
  setf s Bindings.Settings.rho settings.rho;
  setf s Bindings.Settings.rho_is_vec (osqp_int_of_bool settings.rho_is_vec);
  setf s Bindings.Settings.sigma settings.sigma;
  setf s Bindings.Settings.alpha settings.alpha;
  setf s Bindings.Settings.cg_max_iter (ospq_int_of_int settings.cg_max_iter);
  setf s Bindings.Settings.cg_tol_reduction
    (ospq_int_of_int settings.cg_tol_reduction);
  setf s Bindings.Settings.cg_tol_fraction settings.cg_tol_fraction;
  setf s Bindings.Settings.adaptive_rho (ospq_int_of_int settings.adaptive_rho);
  setf s Bindings.Settings.adaptive_rho_interval
    (ospq_int_of_int settings.adaptive_rho_interval);
  setf s Bindings.Settings.adaptive_rho_fraction settings.adaptive_rho_fraction;
  setf s Bindings.Settings.adaptive_rho_tolerance
    settings.adaptive_rho_tolerance;
  setf s Bindings.Settings.max_iter (ospq_int_of_int settings.max_iter);
  setf s Bindings.Settings.eps_abs settings.eps_abs;
  setf s Bindings.Settings.eps_rel settings.eps_rel;
  setf s Bindings.Settings.eps_prim_inf settings.eps_prim_inf;
  setf s Bindings.Settings.eps_dual_inf settings.eps_dual_inf;
  setf s Bindings.Settings.scaled_termination
    (osqp_int_of_bool settings.scaled_termination);
  setf s Bindings.Settings.check_termination
    (ospq_int_of_int settings.check_termination);
  setf s Bindings.Settings.check_dualgap
    (osqp_int_of_bool settings.check_dualgap);
  setf s Bindings.Settings.time_limit settings.time_limit;
  setf s Bindings.Settings.delta settings.delta;
  setf s Bindings.Settings.polish_refine_iter
    (ospq_int_of_int settings.polish_refine_iter);
  s_ptr

let cleanup t =
  Bindings.osqp_cleanup t.solver;
  Bindings.osqp_csc_matrix_free t.qp.p.ptr;
  Bindings.osqp_csc_matrix_free t.qp.a.ptr;
  Bindings.osqp_settings_free t.settings

let define_qp ~p ~q ~a ~l ~u =
  let n = Array.length q in
  let m = Array.length l in
  let* p =
    check_symmetric p
    |> Result.map (fun p -> upper_triangular p |> csc_of_dense |> osqp_of_csc)
  in
  let a = csc_of_dense a |> osqp_of_csc in
  let q, _ = floats q in
  let l, _ = floats l in
  let u, _ = floats u in
  Ok { m; n; p; a; q; l; u }

let setup ?(settings = default_settings) ~p ~q ~a ~l ~u () =
  let settings = configure_settings settings in
  let* qp = define_qp ~p ~q ~a ~l ~u in
  let solver_ptr =
    allocate (ptr Bindings.osqp_solver) (from_voidp Bindings.osqp_solver null)
  in
  let exitflag =
    Bindings.osqp_setup solver_ptr qp.p.ptr (CArray.start qp.q) qp.a.ptr
      (CArray.start qp.l) (CArray.start qp.u) (Int64.of_int qp.m)
      (Int64.of_int qp.n) settings
  in
  if exitflag = 0L then begin
    let solver = !@solver_ptr in
    let t = { solver; qp } in
    Gc.finalise (fun t -> cleanup t) t;
    Ok t
  end
  else Error (Setup_failed (Int64.to_int exitflag))

let extract_solution solver m n =
  let solution = make Bindings.osqp_solution in
  let x_arr = CArray.make double n in
  let y_arr = CArray.make double m in
  let pic_arr = CArray.make double n in
  let dic_arr = CArray.make double m in
  setf solution Bindings.Solution.x (CArray.start x_arr);
  setf solution Bindings.Solution.y (CArray.start y_arr);
  setf solution Bindings.Solution.prim_inf_cert (CArray.start pic_arr);
  setf solution Bindings.Solution.dual_inf_cert (CArray.start dic_arr);
  let exitflag = Bindings.osqp_get_solution solver (addr solution) in
  if exitflag = 0L then begin
    let x = Array.init n (fun i -> CArray.get x_arr i) in
    let y = Array.init m (fun i -> CArray.get y_arr i) in
    ignore (pic_arr, dic_arr);
    Ok { x; y }
  end
  else Error (Solution_failed (Int64.to_int exitflag))

let solve t =
  let exitflag = Bindings.osqp_solve t.solver in
  if exitflag = 0L then begin
    let info = getf !@(t.solver) Bindings.Solver.info in
    let status =
      getf !@info Bindings.Info.status_val |> Int64.to_int |> status_of_int
    in
    match status with
    | Solved ->
        let* solution = extract_solution t.solver t.qp.m t.qp.n in
        Ok solution
    | s -> Error (Solve_failed s)
  end
  else Error (Solve_failed (Unknown (Int64.to_int exitflag)))

let warm_start t ?x ?y () =
  let x = Option.map floats x in
  let y = Option.map floats y in
  let exitflag =
    Bindings.osqp_warm_start t.solver (Option.map snd x) (Option.map snd y)
    |> Int64.to_int
  in
  ignore (x, y);
  if exitflag = 0 then Ok () else Error (Warm_start_failed exitflag)

let update_vectors t ?q ?l ?u () =
  let floats_opt = Option.map floats in
  let q = floats_opt q in
  let l = floats_opt l in
  let u = floats_opt u in
  let exitflag =
    Bindings.osqp_update_data_vec t.solver (Option.map snd q) (Option.map snd l)
      (Option.map snd u)
  in
  ignore (q, l, u);
  if exitflag = 0L then Ok ()
  else Error (Update_vectors_failed (Int64.to_int exitflag))

let update_matrices t ?p ?a () =
  let length_or_zero = function
    | Some (ca, _) -> Int64.of_int (CArray.length ca)
    | None -> 0L
  in
  let px =
    Option.map
      (fun p -> upper_triangular p |> csc_of_dense |> fun c -> floats c.values)
      p
  in
  let ax = Option.map (fun a -> csc_of_dense a |> fun c -> floats c.values) a in
  let exitflag =
    Bindings.osqp_update_data_mat t.solver (Option.map snd px) None
      (length_or_zero px) (Option.map snd ax) None (length_or_zero ax)
  in
  ignore (px, ax);
  if exitflag = 0L then Ok ()
  else Error (Update_matrices_failed (Int64.to_int exitflag))
