open Ctypes
open Foreign
open Bindings 

type csc_matrix = {
  nrows : int;
  ncols : int;
  values : float array;
  row_indices : int array;
  col_pointers : int array;
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

let to_carray ~t arr =
  let n = Array.length arr in
  let ca = CArray.make t n in
  Array.iteri (fun i v -> CArray.set ca i v) arr;
  (ca, CArray.start ca)

let nnz t = Array.length t.values

let csc_of_dense rows =
  let nrows = Array.length rows in
  let ncols = Array.length rows.(0) in
  let rec scan_col j vals idxs ptrs =
    if j >= ncols then
      (List.rev vals, List.rev idxs, List.rev (List.length vals :: ptrs))
    else
      let rec scan_row i vals idxs =
        if i >= nrows then (vals, idxs)
        else if rows.(i).(j) <> 0.0 then
          scan_row (i + 1) (rows.(i).(j) :: vals) (i :: idxs)
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

let define_problem ~p ~q ~a ~l ~u = 
  let p = csc_of_dense p in
  let a = csc_of_dense a in

  let p_x, p_x_ptr = to_carray t:osqp_float p.values in
  let p_i, p_i_ptr = to_carray t:osqp_int p.row_indices in
  let p_p, p_p_ptr = to_carray t:osqp_int p.col_pointers in
  let p_nnz = nnz p in


  let a_x, a_x_ptr = to_carray t:osqp_float a.values in
  let a_i, a_i_ptr = to_carray t:osqp_int a.row_indices in
  let a_p, a_p_ptr = to_carray t:osqp_int a.col_pointers in
  let a_nnz = nnz a in
  
  let n = p.ncols in
  let m = a.nrows in

  let p = osqp_csc_matrix_new n n p_nnz p_x_ptr p_i_ptr p_p_ptr in
  let a = osqp_csc_matrix_new m n a_nnz a_x_ptr a_i_ptr a_p_ptr in
  

let setup_settings = ()
let setup_solver = ()
let extract_solution = ()

let solve ?(settings : default_settings) ~p ~q ~a ~l ~u = ()
(* define *)
(* setup *)
(* solve *)
(* cleanup *)
