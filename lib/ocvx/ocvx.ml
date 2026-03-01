module Var = struct
  type t = { id : int; dim : int }

  let next_id = ref 0

  let make dim =
    let id = !next_id in
    incr next_id;
    { id; dim }
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

type error = Osqp_error of Osqp.error | Scs_error of Scs.error

module Expr = struct
  let var v = Var v
  let const a = Const a
  let scalar f = Scalar f
  let ( + ) a b = Add (a, b)
  let ( - ) a b = Sub (a, b)
  let ( *. ) s e = Smul (s, e)
  let dot c e = Dot (c, e)
  let mat_mul a e = MatMul (a, e)
  let quad_form e p = Quad_form (e, p)
  let norm2 e = Norm2 e
  let sum_eq e = Sum_sq e
  let abs e = Abs e
  let log e = Log e
  let sqrt e = Sqrt e
  let neg_ccx e = Neg_cvx e
  let neg_ccv e = Neg_ccv e
  let neg_aff e = Neg_aff e
end

type constraint_ =
  | Leq :
      [< `Convex | `Affine ] expr * [< `Concave | `Affine ] expr
      -> constraint_
  | Geq :
      [< `Concave | `Affine ] expr * [< `Convex | `Affine ] expr
      -> constraint_
  | Eq : [ `Affine ] expr * [ `Affine ] expr -> constraint_

module Constraint = struct
  type t = constraint_

  let ( <= ) a b = Leq (a, b)
  let ( >= ) a b = Geq (a, b)
  let ( == ) a b = Eq (a, b)
end

type problem =
  | Minimize : [< `Convex | `Affine ] expr * Constraint.t list -> problem
  | Maximize : [< `Concave | `Affine ] expr * Constraint.t list -> problem

module Problem = struct
  type t = problem

  let is_qp p = ()
  let compile_qp = ()
  let compile_conic = ()
  let minimize o c = Minimize (o, c)
  let maximize o c = Maximize (o, c)

  let solve p =
    if is_qp p then compile_qp p |> run_osqp else compile_conic p |> run_scs
end

let ones n = Array.make n 1.0
