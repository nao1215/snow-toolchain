open Alcotest
open Ast
open Advanced_typechecker

(** Test basic type inference for literals *)
let test_literal_inference () =
  let test_cases =
    [
      (EInt 42L, "int");
      (EFloat 3.14, "float");
      (EBool true, "bool");
      (EString "hello", "string");
      (EUnit, "unit");
    ]
  in

  List.iter
    (fun (expr, expected) ->
      match infer_expression_type expr with
      | Ok inferred_type ->
          let actual = TypeUtils.typ_to_string inferred_type in
          check string
            (Printf.sprintf "Infer type of %s" expected)
            expected actual
      | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg))
    test_cases

(** Test type inference for None with automatic Option type *)
let test_none_inference () =
  (* ENone should infer as Option<'0> where '0 is a fresh type variable *)
  match infer_expression_type ENone with
  | Ok (TOption (TVar _)) ->
      check bool "ENone infers as Option with type variable" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected Option<'a>, got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test type inference for Ok expressions *)
let test_ok_inference () =
  (* EOk(42) should infer as Result<int, 'e> *)
  match infer_expression_type (EOk (EInt 42L)) with
  | Ok (TResult (TInt, TVar _)) ->
      check bool "EOk(int) infers as Result<int, 'a>" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected Result<int, 'a>, got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test type inference for Some expressions *)
let test_some_inference () =
  (* ESome(42) should infer as Option<int> *)
  match infer_expression_type (ESome (EInt 42L)) with
  | Ok (TOption TInt) -> check bool "ESome(int) infers as Option<int>" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected Option<int>, got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test type inference for binary operations *)
let test_binop_inference () =
  let test_cases =
    [
      (EBinOp (Add, EInt 1L, EInt 2L), TInt, "1 + 2 should infer as int");
      ( EBinOp (Add, EString "a", EString "b"),
        TString,
        "string concatenation should infer as string" );
      (EBinOp (Eq, EInt 1L, EInt 2L), TBool, "1 == 2 should infer as bool");
      ( EBinOp (Lt, EFloat 1.0, EFloat 2.0),
        TBool,
        "1.0 < 2.0 should infer as bool" );
    ]
  in

  List.iter
    (fun (expr, expected, desc) ->
      match infer_expression_type expr with
      | Ok inferred_type ->
          check bool desc true (TypeUtils.typ_equal inferred_type expected)
      | Error msg ->
          fail (Printf.sprintf "Type inference failed for %s: %s" desc msg))
    test_cases

(** Test type inference for let expressions with polymorphism *)
let test_let_polymorphism () =
  (* let id = fun(x) -> x in id *)
  let param = { name = "x"; param_type = TVar 999 } in
  (* Use placeholder, should be inferred *)
  let identity_func = EFun ([ param ], TVar 999, EVar "x") in
  let let_expr = ELet ("id", identity_func, EVar "id") in

  match infer_expression_type let_expr with
  | Ok (TFunc ([ TVar _ ], TVar _)) ->
      check bool "Identity function infers with polymorphic types" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected polymorphic function type, got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test type inference for arrays *)
let test_array_inference () =
  (* [1, 2, 3] should infer as [int; 3] *)
  let array_expr = EArray [ EInt 1L; EInt 2L; EInt 3L ] in

  match infer_expression_type array_expr with
  | Ok (TArray (TInt, Some 3)) ->
      check bool "Integer array infers correctly" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected [int; 3], got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test type inference for tuples *)
let test_tuple_inference () =
  (* (1, "hello", true) should infer as (int, string, bool) *)
  let tuple_expr = ETuple [ EInt 1L; EString "hello"; EBool true ] in

  match infer_expression_type tuple_expr with
  | Ok (TTuple [ TInt; TString; TBool ]) ->
      check bool "Tuple infers correctly" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected (int, string, bool), got %s"
           (TypeUtils.typ_to_string actual_type))
  | Error msg -> fail (Printf.sprintf "Type inference failed: %s" msg)

(** Test unification errors *)
let test_unification_errors () =
  (* 1 + "string" should fail *)
  let invalid_expr = EBinOp (Add, EInt 1L, EString "test") in

  match infer_expression_type invalid_expr with
  | Error _ ->
      check bool "Type error correctly detected for 1 + string" true true
  | Ok actual_type ->
      fail
        (Printf.sprintf "Expected type error, but got %s"
           (TypeUtils.typ_to_string actual_type))

let () =
  run "Type Inference Tests"
    [
      ( "literal inference",
        [ test_case "literal types" `Quick test_literal_inference ] );
      ( "option/result inference",
        [
          test_case "ENone inference" `Quick test_none_inference;
          test_case "EOk inference" `Quick test_ok_inference;
          test_case "ESome inference" `Quick test_some_inference;
        ] );
      ( "binary operations",
        [ test_case "binary operation inference" `Quick test_binop_inference ]
      );
      ( "polymorphism",
        [ test_case "let polymorphism" `Quick test_let_polymorphism ] );
      ( "compound types",
        [
          test_case "array inference" `Quick test_array_inference;
          test_case "tuple inference" `Quick test_tuple_inference;
        ] );
      ( "error handling",
        [ test_case "unification errors" `Quick test_unification_errors ] );
    ]
