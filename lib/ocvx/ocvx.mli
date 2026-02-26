module Var : sig
  type t

  val create : int -> t
end

type _ expr =
  (* Affine atoms — produce [> `Affine], accepted everywhere *)
  | Var : Var.t -> [> `Affine ] expr
  | Const : float array -> [> `Affine ] expr
  | Scalar : float -> [> `Affine ] expr
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Smul : float * 'a expr -> 'a expr
  | Dot : float array * [> `Affine ] expr -> [> `Affine ] expr
  | MatMul : float array array * [> `Affine ] expr -> [> `Affine ] expr
  (* Convex atoms — require affine input, produce convex *)
  | Quad_form : [ `Affine ] expr * float array array -> [> `Convex ] expr
  | Norm2 : [ `Affine ] expr -> [> `Convex ] expr
  | Sum_sq : [ `Affine ] expr -> [> `Convex ] expr
  | Abs : [ `Affine ] expr -> [> `Convex ] expr
  (* Concave atoms — require affine input, produce concave *)
  | Log : [ `Affine ] expr -> [> `Concave ] expr
  | Sqrt : [ `Affine ] expr -> [> `Concave ] expr
  (* Negation flips curvature *)
  | Neg_cvx : [> `Convex ] expr -> [> `Concave ] expr
  | Neg_ccv : [> `Concave ] expr -> [> `Convex ] expr
  | Neg_aff : [ `Affine ] expr -> [> `Affine ] expr

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
  type t

  val minimize : [< `Convex | `Affine ] expr -> Constraint.t list -> t
  val maximize : [< `Concave | `Affine ] expr -> Constraint.t list -> t
  val solve : solver:[ `Osqp | `Scs ] -> t -> (float array, error) result
end

val ones : int -> float array
