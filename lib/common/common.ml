open Ctypes

let to_carray ~t ~f arr =
  let n = Array.length arr in
  let ca = CArray.make t n in
  Array.iteri (fun i v -> CArray.set ca i (f v)) arr;
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
