module Var : sig
  type t

  val make : int -> t
end

type _ expr
type error

module Expr : sig
  val var : Var.t -> [> `Affine ] expr
  val const : float array -> [> `Affine ] expr
  val scalar : float -> [> `Affine ] expr
  val ( + ) : 'a expr -> 'a expr -> 'a expr
  val ( - ) : 'a expr -> 'a expr -> 'a expr
  val ( *. ) : float -> 'a expr -> 'a expr
  val dot : float array -> [> `Affine ] expr -> [ `Affine ] expr
  val mat_mul : float array array -> [> `Affine ] expr -> [> `Affine ] expr
  val quad_form : [ `Affine ] expr -> float array array -> [> `Convex ] expr
  val norm2 : [ `Affine ] expr -> [> `Convex ] expr
  val sum_eq : [ `Affine ] expr -> [> `Convex ] expr
  val abs : [ `Affine ] expr -> [> `Convex ] expr
  val log : [ `Affine ] expr -> [> `Concave ] expr
  val sqrt : [> `Affine ] expr -> [> `Concave ] expr
  val neg_ccx : [> `Convex ] expr -> [> `Concave ] expr
  val neg_ccv : [> `Concave ] expr -> [> `Convex ] expr
  val neg_aff : [> `Affine ] expr -> [> `Affine ] expr
end

module Constraint : sig
  type t

  val ( <= ) : [< `Convex | `Affine ] expr -> [< `Concave | `Affine ] expr -> t
  val ( >= ) : [< `Concave | `Affine ] expr -> [< `Convex | `Affine ] expr -> t
  val ( == ) : [ `Affine ] expr -> [ `Affine ] expr -> t
end

module Problem : sig
  type t =
    | Minimize : [< `Convex | `Affine ] expr * Constraint.t list -> t
    | Maximize : [< `Concave | `Affine ] expr * Constraint.t list -> t

  type compiled

  val minimize : [< `Convex | `Affine ] expr -> Constraint.t list -> t
  val maximize : [< `Concave | `Affine ] expr -> Constraint.t list -> t
  val compile : t -> (compiled, error) result
  val solve : t -> (float array, error) result
  val recompile : compiled -> t -> (compiled, error) result
end

val ones : int -> float array
