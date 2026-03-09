type error = Osqp_error of Osqp.error | Scs_error of Scs.error

module Var = struct
  type t = { id : int; dim : int; index : int option }

  let next_id = ref 0

  let make dim =
    let id = !next_id in
    incr next_id;
    { id; dim; index = None }
end

module Param = struct
  type t = { id : int; name : string; value : float array array }

  let next_id = ref 0

  let make name value =
    let id = !next_id in
    incr next_id;
    { id; name; value }

  let set p value = { p with value }
end

module Solution = struct 
  type solution = {
    x      : float array;
    duals  : float array;
    slacks : float array option;
  }

  let get sol var =
    match var.index with
    | None -> invalid_arg "get: variable has no assigned index"
    | Some idx -> Array.sub sol.x idx var.dim

  let make x y ?s () = { x; duals = y; slacks = s }

  let of_osqp (sol: Osqp.solution) = make sol.x sol.y ()

  let of_scs (sol: Scs.solution) = make sol.x sol.y sol.s ()
end

module Var_set = Set.Make(struct 
  type t = Var.t
  let compare a b = Int.compare a.id b.id
end)

(* Closed variant tags — affine is a subtype of both convex and concave *)
type affine = [ `Affine ]
type convex = [ `Affine | `Convex ]
type concave = [ `Affine | `Concave ]

type _ expr =
  (* Affine atoms *)
  | Var : Var.t -> affine expr
  | Param : Param.t -> affine expr
  | Const : float array -> affine expr
  | Scalar : float -> affine expr
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Smul : float * 'a expr -> 'a expr
  | Dot : float array * affine expr -> affine expr
  | MatMul : float array array * affine expr -> affine expr
  (* Convex atoms — affine input only (nonmonotone) *)
  | Quad_form : affine expr * float array array -> convex expr
  | Norm2 : affine expr -> convex expr
  | Sum_sq : affine expr -> convex expr
  | Abs : affine expr -> convex expr
  (* Concave atoms — accept affine or concave input (increasing) *)
  | Log : concave expr -> concave expr
  | Sqrt : concave expr -> concave expr
  (* Negation — three constructors preserve static curvature *)
  | Neg_aff : affine expr -> affine expr
  | Neg_cvx : convex expr -> concave expr
  | Neg_ccv : concave expr -> convex expr

module Expr = struct
  let var dim = Var (Var.make dim)
  let param name value = Param (Param.make name value)
  let const a = Const a
  let scalar f = Scalar f
  let ( + ) a b = Add (a, b)
  let ( - ) a b = Sub (a, b)

  let ( *. ) s e =
    if s >= 0.0 then Smul (s, e)
    else invalid_arg "Smul: negative scalar, use neg/neg_cvx/neg_ccv explicitly"

  let dot c e = Dot (c, e)
  let mat_mul a e = MatMul (a, e)

  let quad_form e p =
    (* TODO: remove and let the BE handle it *)
    let p = p |> assert_symmetric |> assert_psd in
    Quad_form (e, p)

  let norm2 e = Norm2 e
  let sum_sq e = Sum_sq e
  let abs e = Abs e
  let log e = Log e
  let sqrt e = Sqrt e
  let neg e = Neg_aff e
  let neg_ccx e = Neg_cvx e
  let neg_ccv e = Neg_ccv e
end

type constraint_ =
  | Leq : convex expr * concave expr -> constraint_
  | Geq : concave expr * convex expr -> constraint_
  | Eq : affine expr * affine expr -> constraint_

module Constraint = struct
  type t = constraint_

  let ( <= ) a b = Leq (a, b)
  let ( >= ) a b = Geq (a, b)
  let ( == ) a b = Eq (a, b)
end

module Compiler = struct 
  type solver_problem =
    | QP of {
        p : float array array;
        q : float array;
        a : float array array;
        l : float array;
        u : float array;
      }
    | Conic of {
        p : float array array;
        c : float array;
        a : float array array;
        b : float array;
        cone : Scs.cone;
      }

  type qp_coeffs = {
    p : float array array;
    q : float array;
  }



  let rec is_qp_expr : type a. a expr -> bool = function
    | Var _ | Const _ | Scalar _ | Param _ -> true
    | Add (a, b) | Sub (a, b) -> is_qp_expr a && is_qp_expr b
    | Smul (_, e) -> is_qp_expr e
    | Dot (_, e) | MatMul (_, e) -> is_qp_expr e
    | Quad_form _ -> true
    | Sum_sq e | Neg_aff e -> is_qp_expr e
    | Norm2 _ | Abs _ | Log _ | Sqrt _ -> false
    | Neg_cvx _ | Neg_ccv _ -> false

  let is_qp_constraint = function
    | Leq (a, b) -> is_qp_expr a && is_qp_expr b
    | Geq (a, b) -> is_qp_expr a && is_qp_expr b
    | Eq (a, b) -> is_qp_expr a && is_qp_expr b

  let is_qp = function
    | Minimize (obj, constrs) ->
        is_qp_expr obj && List.for_all is_qp_constraint constrs
    | Maximize _ -> false

  let collect_vars expr =
    let rec aux : type a. a expr -> Var.t list -> Var.t list =
     fun e acc ->
      match e with
      | Var v ->
          Var_set.add v acc
      | Param _ | Const _ | Scalar _ -> acc
      | Add (a, b) | Sub (a, b) -> aux a acc |> aux b
      | Smul (_, e) -> aux e acc
      | Dot (_, e) | MatMul (_, e) -> aux e acc
      | Quad_form (e, _) | Norm2 e | Sum_sq e | Abs e -> aux e acc
      | Log e | Sqrt e -> aux e acc
      | Neg_cvx e -> aux e acc
      | Neg_ccv e -> aux e acc
      | Neg_aff e -> aux e acc
    in
    aux expr Var_set.empty |> Var_set.elements

    let blit_block dst src idx =
      for i = 0 to Array.length src - 1 do
        for j = 0 to Array.length src.(0) - 1 do
          dst.(idx + i).(idx + j) <- src.(i).(j)
        done
      done

  let extract_objective : type a. a expr -> qp_coeffs -> qp_coeffs =
    fun e acc -> match e with
      | Quad_form (Var v, p) -> blit_block acc.p p v.index

  let extract_qp (Minimize (obj, constrs)) =
    let vars = Var_set.union (collect_vars obj) (collect_vars constrs) in
    

  let compile p = 
    if is_qp p then begin
      let compiled = extract_qp p in
    end
    else begin

      end

  (* let compile_qp = () *)
  (* let compile_conic = () *)

end

(* module Problem = struct *)
(**)
(*   type t = *)
(*     | Minimize : convex expr * Constraint.t list -> t *)
(*     | Maximize : concave expr * Constraint.t list -> t *)
(**)
(*   (* let minimize o c = Minimize (o, c) *) *)
(*   (* let maximize o c = Maximize (o, c) *) *)
(*   (* let compile p = () *) *)
(*   (**) *)
(*   (* let solve p = *) *)
(*   (*   if is_qp p then compile_qp p |> run_osqp else compile_conic p |> run_scs *) *)
(*   (**) *)
(*   (* let recompile c p = () *) *)
(* end *)
(**)
(* let ones n = Array.make n 1.0 *)
