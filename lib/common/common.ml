open Ctypes

module Csc = struct
  type t = {
    nrows : int;
    ncols : int;
    values : float array;
    row_indices : int array;
    col_pointers : int array;
  }

  let of_dense ?(eps = 0.0) m =
    let non_zero msg n = if n > 0 then n else invalid_arg msg in
    let nrows = Array.length m |> non_zero "Csc.of_dense: matrix has no rows" in
    let ncols =
      Array.length m.(0) |> non_zero "Csc.of_dense: matrix has no cols"
    in
    let vals = ref [] in
    let ridxs = ref [] in
    let cptrs = Array.make (ncols + 1) 0 in
    for j = 0 to ncols - 1 do
      cptrs.(j) <- List.length !vals;
      for i = 0 to nrows - 1 do
        if Float.abs m.(i).(j) > eps then begin
          vals := m.(i).(j) :: !vals;
          ridxs := i :: !ridxs
        end
      done
    done;
    cptrs.(ncols) <- List.length !vals;
    Ok
      {
        nrows;
        ncols;
        values = Array.of_list (List.rev !vals);
        row_indices = Array.of_list (List.rev !ridxs);
        col_pointers = cptrs;
      }

  let nnz t = Array.length t.values
end

let to_carray ~t ~f arr =
  let n = Array.length arr in
  let ca = CArray.make t n in
  Array.iteri (fun i v -> CArray.set ca i (f v)) arr;
  (ca, CArray.start ca)

let to_upper_triangular m =
  let n = Array.length m in
  Array.init n (fun i ->
      Array.init n (fun j -> if j >= i then m.(i).(j) else 0.0))

let cholesky m =
  let n = Array.length m in
  let l = Array.init n (fun _ -> Array.make n 0.0) in
  for j = 0 to n - 1 do
    let sum = ref 0.0 in
    for k = 0 to j - 1 do
      sum := !sum +. (l.(j).(k) *. l.(j).(k))
    done;
    let diag = m.(j).(j) -. !sum in
    if diag <= 0.0 then failwith "not positive definite";
    l.(j).(j) <- Float.sqrt diag;
    for i = j + 1 to n - 1 do
      let sum = ref 0.0 in
      for k = 0 to j - 1 do
        sum := !sum +. (l.(i).(k) *. l.(j).(k))
      done;
      l.(i).(j) <- (m.(i).(j) -. !sum) /. l.(j).(j)
    done
  done;
  l

let assert_symmetric m =
  let n = Array.length m in
  let rec check i j =
    if i >= n then m
    else if j >= n then check (i + 1) (i + 2)
    else if Float.abs (m.(i).(j) -. m.(j).(i)) > 1e-10 then
      invalid_arg
        (Printf.sprintf "assert_symmetric: not symmetric at (%d,%d): %f != %f" i
           j
           m.(i).(j)
           m.(j).(i))
    else check i (j + 1)
  in
  check 0 1

let is_psd m =
  try
    cholesky m |> ignore;
    true
  with Failure _ -> false

let assert_psd m =
  if not (is_psd m) then
    invalid_arg "assert_psd: matrix is not positive semidefinite";
  m
