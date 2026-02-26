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

type error = 
