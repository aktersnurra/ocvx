open Ctypes
open Foreign
open Result.Syntax

type csc_matrix = {
  nrows : int;
  ncols : int;
  values : float array;
  row_indices : int array;
  col_pointers : int array;
}

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
  settings : Bindings.osqp_settings structure ptr;
  qp : qp;
}

type settings = {
  alpha : float;
  verbose : bool;
  max_iter : int;
  eps_abs : float;
  eps_rel : float;
  polish : bool;
}

let default_settings =
  {
    alpha = 1.6;
    verbose = true;
    max_iter = 4_000;
    eps_abs = 1e-3;
    eps_rel = 1e-3;
    polish = false;
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

let float_carray_of_array arr =
  let n = Array.length arr in
  let ca = CArray.make Bindings.osqp_float n in
  Array.iteri (fun i v -> CArray.set ca i v) arr;
  (ca, CArray.start ca)

let int_carray_of_array arr =
  let n = Array.length arr in
  let ca = CArray.make Bindings.osqp_int n in
  Array.iteri (fun i v -> CArray.set ca i (Int64.of_int v)) arr;
  (ca, CArray.start ca)

let nnz csc = Array.length csc.values

let upper_triangular m =
  let n = Array.length m in
  Array.init n (fun i ->
      Array.init n (fun j -> if j >= i then m.(i).(j) else 0.0))

let csc_of_dense m =
  let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  let rec scan_col j vals idxs ptrs =
    if j >= ncols then
      (List.rev vals, List.rev idxs, List.rev (List.length vals :: ptrs))
    else
      let rec scan_row i vals idxs =
        if i >= nrows then (vals, idxs)
        else if m.(i).(j) <> 0.0 then
          scan_row (i + 1) (m.(i).(j) :: vals) (i :: idxs)
        else scan_row (i + 1) vals idxs
      in
      let vals', idxs' = scan_row 0 vals idxs in
      scan_col (j + 1) vals' idxs' (List.length vals :: ptrs)
  in
  let vals, idxs, ptrs = scan_col 0 [] [] [] in
  {
    nrows;
    ncols;
    values = Array.of_list vals;
    row_indices = Array.of_list idxs;
    col_pointers = Array.of_list ptrs;
  }

let osqp_of_csc csc =
  let x, x_ptr = float_carray_of_array csc.values in
  let i, i_ptr = int_carray_of_array csc.row_indices in
  let p, p_ptr = int_carray_of_array csc.col_pointers in
  let ptr =
    Bindings.osqp_csc_matrix_new (Int64.of_int csc.nrows)
      (Int64.of_int csc.ncols)
      (Int64.of_int (nnz csc))
      x_ptr i_ptr p_ptr
  in
  { ptr; _x = x; _i = i; _p = p }

let configure_settings settings =
  let s_ptr = Bindings.osqp_settings_new () in
  Bindings.osqp_set_default_settings s_ptr;
  if settings <> default_settings then begin
    let s = !@s_ptr in
    setf s Bindings.Settings.verbose
      (Int64.of_int (if settings.verbose then 1 else 0));
    setf s Bindings.Settings.alpha settings.alpha;
    setf s Bindings.Settings.max_iter (Int64.of_int settings.max_iter);
    setf s Bindings.Settings.eps_abs settings.eps_abs;
    setf s Bindings.Settings.eps_rel settings.eps_rel;
    setf s Bindings.Settings.polishing
      (Int64.of_int (if settings.polish then 1 else 0))
  end;
  s_ptr

let cleanup t =
  Bindings.osqp_cleanup t.solver;
  Bindings.osqp_csc_matrix_free t.qp.p.ptr;
  Bindings.osqp_csc_matrix_free t.qp.a.ptr;
  Bindings.osqp_settings_free t.settings

let define_qp ~p ~q ~a ~l ~u =
  let n = Array.length q in
  let m = Array.length l in
  let p = upper_triangular p |> csc_of_dense |> osqp_of_csc in
  let a = csc_of_dense a |> osqp_of_csc in
  let q, _ = float_carray_of_array q in
  let l, _ = float_carray_of_array l in
  let u, _ = float_carray_of_array u in
  { m; n; p; a; q; l; u }

let setup ?(settings = default_settings) ~p ~q ~a ~l ~u () =
  let settings = configure_settings settings in
  let qp = define_qp ~p ~q ~a ~l ~u in
  let solver_ptr =
    allocate (ptr Bindings.osqp_solver) (from_voidp Bindings.osqp_solver null)
  in
  let exitflag =
    Bindings.osqp_setup solver_ptr qp.p.ptr (CArray.start qp.q) qp.a.ptr
      (CArray.start qp.l) (CArray.start qp.u) (Int64.of_int qp.m)
      (Int64.of_int qp.n) settings
    |> Int64.to_int
  in
  match exitflag with
  | 0 ->
      let solver = !@solver_ptr in
      let t = { solver; settings; qp } in
      Gc.finalise (fun t -> cleanup t) t;
      Ok t
  | e -> Error (Setup_failed e)

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
  let exitflag =
    Bindings.osqp_get_solution solver (addr solution) |> Int64.to_int
  in
  match exitflag with
  | 0 ->
      let x = Array.init n (fun i -> CArray.get x_arr i) in
      let y = Array.init m (fun i -> CArray.get y_arr i) in
      ignore (pic_arr, dic_arr);
      Ok { x; y }
  | i -> Error (Solution_failed i)

let solve t =
  let exitflag = Bindings.osqp_solve t.solver |> Int64.to_int in
  match exitflag with
  | 0 -> (
      let info = getf !@(t.solver) Bindings.Solver.info in
      let status =
        getf !@info Bindings.Info.status_val |> Int64.to_int |> status_of_int
      in
      match status with
      | Solved ->
          let* solution = extract_solution t.solver t.qp.m t.qp.n in
          Ok solution
      | s -> Error (Solve_failed s))
  | i -> Error (Solve_failed (Unknown i))

let warm_start t ~x ~y = 

