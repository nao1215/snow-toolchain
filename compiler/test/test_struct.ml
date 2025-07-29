(* Debug: print type constructor info *)
let () = Printf.printf "Testing struct type construction...\n"

open Alcotest
open Ast
open Typechecker

(* Test basic field creation *)
let test_field = { field_name = "x"; field_type = TInt }

let () =
  Printf.printf "Field created: %s : %s\n" test_field.field_name
    (TypeUtils.typ_to_string test_field.field_type)

(** Test struct type checking *)
let test_struct_typing () =
  (* Create a simple struct type: { x: int, y: int } *)
  let struct_type =
    (TStruct
       [
         { field_name = "x"; field_type = TInt };
         { field_name = "y"; field_type = TInt };
       ]
      : typ)
  in

  (* Test field access type checking *)
  let struct_var = EVar "point" in
  let field_access = EFieldAccess (struct_var, "x") in

  (* Create environment with struct variable *)
  let env = extend_env empty_env "point" struct_type in

  (* Check that field access type checks correctly *)
  try
    let result_type = infer_type env field_access in
    match result_type with
    | TInt -> check bool "field access returns correct type" true true
    | other_type ->
        fail
          (Printf.sprintf "Expected TInt, got %s"
             (TypeUtils.typ_to_string other_type))
  with Type_error msg -> fail (Printf.sprintf "Type checking failed: %s" msg)

(** Test struct literal creation (basic version) *)
let test_struct_literal_typing () =
  (* Test struct literal: { x = 10, y = 20 } *)
  let struct_literal =
    EStruct
      [
        { init_field_name = "x"; init_value = EInt 10L };
        { init_field_name = "y"; init_value = EInt 20L };
      ]
  in

  (* Since struct literals require type annotation in current implementation,
     this should fail with appropriate error message *)
  try
    let _ = infer_type empty_env struct_literal in
    fail "Expected struct literal to require type annotation"
  with
  | Type_error "Struct literals require type annotation" ->
      check bool "struct literal requires type annotation" true true
  | Type_error other_msg ->
      fail (Printf.sprintf "Expected specific error, got: %s" other_msg)

let () =
  run "Struct Tests"
    [
      ( "struct typing",
        [
          test_case "struct field access type checking" `Quick
            test_struct_typing;
        ] );
      ( "struct literal",
        [
          test_case "struct literal type checking" `Quick
            test_struct_literal_typing;
        ] );
    ]
