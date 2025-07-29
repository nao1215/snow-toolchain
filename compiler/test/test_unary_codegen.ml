open Alcotest
open Ast
open Codegen

(** Test unary negation operator *)
let test_unary_neg () =
  (* Test: -42 *)
  let expr = EUnOp (Neg, EInt 42L) in

  try
    let ir = generate_llvm_ir expr in
    (* Check that IR is generated and contains negation instruction *)
    check bool "unary negation generates IR" true (String.length ir > 0);
    check bool "IR contains negation" true (String.contains ir '-')
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test unary not operator *)
let test_unary_not () =
  (* Test: !true *)
  let expr = EUnOp (Not, EBool true) in

  try
    let ir = generate_llvm_ir expr in
    (* Check that IR is generated *)
    check bool "unary not generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test nested unary operations *)
let test_nested_unary () =
  (* Test: -(-10) *)
  let expr = EUnOp (Neg, EUnOp (Neg, EInt 10L)) in

  try
    let ir = generate_llvm_ir expr in
    check bool "nested unary operations generate IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test unary operations in expressions *)
let test_unary_in_expression () =
  (* Test: let x = -5 in x + 10 *)
  let neg_expr = EUnOp (Neg, EInt 5L) in
  let expr = ELet ("x", neg_expr, EBinOp (Add, EVar "x", EInt 10L)) in

  try
    let ir = generate_llvm_ir expr in
    check bool "unary in expression generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Unary Operations Codegen Tests"
    [
      ("negation", [ test_case "simple negation" `Quick test_unary_neg ]);
      ("logical not", [ test_case "boolean not" `Quick test_unary_not ]);
      ( "nested operations",
        [ test_case "nested unary operations" `Quick test_nested_unary ] );
      ( "expression integration",
        [ test_case "unary in expressions" `Quick test_unary_in_expression ] );
    ]
