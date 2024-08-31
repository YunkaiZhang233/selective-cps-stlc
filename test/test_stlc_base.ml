(* test/test_stlc_b.ml *)
open Alcotest
open Stlc_base

let test_typecheck_var () =
  let env = [("x", BaseT IntT)] in
  let expr = Var "x" in
  let expected = Some (BaseT IntT) in
  check (option (of_pp pp_typ)) "typecheck var" expected (typecheck env expr)

let test_typecheck_abstr () =
  let env = [] in
  let expr = Abstr ("x", BaseT IntT, Var "x") in
  let expected = Some (ArrowT (BaseT IntT, BaseT IntT)) in
  check (option (of_pp pp_typ)) "typecheck abstraction" expected (typecheck env expr)

let test_typecheck_app () =
  let env = [("f", ArrowT (BaseT IntT, BaseT IntT)); ("x", BaseT IntT)] in
  let expr = App (Var "f", Var "x") in
  let expected = Some (BaseT IntT) in
  check (option (of_pp pp_typ)) "typecheck application" expected (typecheck env expr)

let test_typecheck_predef () =
  let env = [] in
  let expr = Predef (Plus (Const (IntC 1), Const (IntC 2))) in
  let expected = Some (BaseT IntT) in
  check (option (of_pp pp_typ)) "typecheck predef Plus" expected (typecheck env expr)

let () =
  run "STLC Tests" [
    "typecheck var", [ test_case "var" `Quick test_typecheck_var ];
    "typecheck abstraction", [ test_case "abstr" `Quick test_typecheck_abstr ];
    "typecheck application", [ test_case "app" `Quick test_typecheck_app ];
    "typecheck predef", [ test_case "predef" `Quick test_typecheck_predef ];
  ]
