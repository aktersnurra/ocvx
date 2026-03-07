open Ctypes
open Foreign

type csc_matrix = {
  nrows : int;
  ncols : int;
  values : float array;
  row_indices : int array;
  col_pointers : int array;
}

module Csc : sig
  type t = {
    nrows : int;
    ncols : int;
    values : float array;
    row_indices : int array;
    col_pointers : int array;
  }

  val make : float array array -> t
  val nnz : t -> int
end

val to_carray :
  t:'a Ctypes.typ ->
  f:('b -> 'a) ->
  'b array ->
  'a Ctypes.carray * 'a Ctypes.ptr

val to_upper_triangular : float array array -> float array array
val assert_symmetric : float array array -> float array array
val assert_psd : float array array -> float array array
