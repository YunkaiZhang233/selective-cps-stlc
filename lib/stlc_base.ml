type typ_base = 
  | IntT

type typ = 
  | BaseT of (typ_base)
  | ArrowT of (typ * typ)

type var = string

type expr_const = 
  | IntC of int

let get_const_type (e : expr_const) : typ_base = 
  match e with
  | IntC _ -> IntT

type expr_predef = 
  | Plus of expr * expr
  | Mult of expr * expr

and expr = 
  | Const of expr_const
  | Var of var
  | Abstr of var * typ * expr
  | App of expr * expr
  | Predef of expr_predef

type context = (var * typ) list

let rec typecheck (env : context) (e : expr) : typ option = 
  match e with
  | Var x -> List.assoc_opt x env
  | Const expr_c -> 
      (
        match expr_c with
        | IntC _ -> Some (BaseT (get_const_type expr_c))
      )
  | Abstr (x, ty_x, e) -> 
      (
        let ty_opt_e = typecheck ((x, ty_x) :: env) e in
        if Option.is_none ty_opt_e then None else (
          let ty_e = Option.get ty_opt_e in
          Some (ArrowT (ty_x, ty_e))
        )
      )
  | App (e1, e2) -> 
      (
        match typecheck env e1, typecheck env e2 with
        | Some (ArrowT (ty_a, ty_b)), Some ty when ty_a = ty -> Some ty_b
        | _ -> None
      )
  | Predef e -> predef_typecheck env e
and predef_typecheck (env : context) (e : expr_predef) : typ option = 
  let check_arithmetic_op env e1 e2 =
    match typecheck env e1, typecheck env e2 with
    | Some (BaseT IntT), Some (BaseT IntT) -> Some (BaseT IntT)
    | _ -> None
  in 
  match e with
  | Plus (e1, e2) -> check_arithmetic_op env e1 e2
  | Mult (e1, e2) -> check_arithmetic_op env e1 e2


(* for testing usage *)
let rec pp_typ fmt = function
  | BaseT IntT -> Format.fprintf fmt "Int"
  | ArrowT (t1, t2) -> Format.fprintf fmt "(%a -> %a)" pp_typ t1 pp_typ t2
