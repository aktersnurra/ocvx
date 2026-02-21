open Ctypes
open Foreign

type csc_matrix = {
  nrows : int;
  ncols : int;
  values : float array;
  row_indices : int array;
  col_pointers : int array;
}

val csc_of_dense : float array array -> csc_matrix
val nnz : csc_matrix -> int
val to_carray : t:'a array -> f:('a -> 'b) -> 'b CArray.t * 'b ptr
val upper_triangular : float array array -> float array array
val check_symmetric: float array array -> (float array array, string) result
