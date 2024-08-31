type base_const =
  | Int of int
  (* or other more instant types *)

(* τ ::= b | τ → τ | ⟨τ⟩ *)
type ty = 
  | Base (* b *)
  | Arrow of ty * ty (* τ → τ *)
  | Cont of ty  (* ⟨τ⟩ *)

type var = string

(* e ::= c | x | fun f x : τ.1 .e | e @ e | callcc x.e | throw e e *)
type expr =
  | Const of base_const
  | Var of var  
  | Abs of var * var * expr
  | App of expr * expr
  | Callcc of var * expr
  | Throw of expr * expr
(* 
type typedExpr = 
  | Const of base_const 

(* typing context *)
type context = (var * ty) list

(* typing rules *)
let rec typeof (ctx: context) (e: expr): ty option =
  match e with
  | Var x -> List.assoc_opt x ctx
  | Const _c -> Some (Base)
  | Abs (f, x, e1) -> None
      (* let ty_x = Base in  (* Assume base type for simplicity *)
      let ty_e1 = typeof ((x, ty_x) :: ctx) e1 in
      (match ty_e1 with
        | Some ty -> Some (Arrow (ty_x, ty))
        | None -> None) *)
  | App (e1, e2) ->
      (match typeof ctx e1, typeof ctx e2 with
        | Some (Arrow (ty1, ty2)), Some ty when ty1 = ty -> Some ty2
        | _ -> None)
  | Callcc (x, e1) -> None
      (* let ty_x = Cont Base in  (* Continuation type *)
      typeof ((x, ty_x) :: ctx) e1 *)
  | Throw (e1, e2) ->
      (match typeof ctx e1, typeof ctx e2 with
        | Some (Cont ty), Some ty' when ty = ty' -> Some ty
        | _ -> None)



         *)