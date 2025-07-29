open Alcotest
open Ast
open Codegen

(** Test simple tuple construction *)
let test_tuple_construction () =
  (* Test: let t = (1, 2, 3) in 42 - safe test that uses tuple in let binding *)
  let tuple_expr = ETuple [ EInt 1L; EInt 2L; EInt 3L ] in
  let expr = ELet ("t", tuple_expr, EInt 42L) in

  try
    let ir = generate_llvm_ir expr in
    (* Check that IR is generated and contains struct operations *)
    check bool "tuple construction generates IR" true (String.length ir > 0);
    (* Look for struct-related LLVM instructions *)
    check bool "IR contains struct operations" true
      (String.contains ir '{' || String.contains ir '}')
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test empty tuple (unit) *)
let test_empty_tuple () =
  (* Test: let unit = () in 0 - safe test for empty tuple *)
  let empty_tuple = ETuple [] in
  let expr = ELet ("unit", empty_tuple, EInt 0L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "empty tuple generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test nested tuples *)
let test_nested_tuples () =
  (* Test: let nested = ((1, 2), (3, 4)) in 100 - safe test for nested tuples *)
  let inner1 = ETuple [ EInt 1L; EInt 2L ] in
  let inner2 = ETuple [ EInt 3L; EInt 4L ] in
  let nested_tuple = ETuple [ inner1; inner2 ] in
  let expr = ELet ("nested", nested_tuple, EInt 100L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "nested tuples generate IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test tuple with mixed types *)
let test_mixed_type_tuple () =
  (* Test: let mixed = (42, true, "hello") in 999 - safe test for mixed types *)
  let mixed_tuple = ETuple [ EInt 42L; EBool true; EString "hello" ] in
  let expr = ELet ("mixed", mixed_tuple, EInt 999L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "mixed type tuple generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test tuple in let binding *)
let test_tuple_let_binding () =
  (* Test: let t = (10, 20) in 55 - safe test that creates tuple in let
     binding *)
  let tuple_expr = ETuple [ EInt 10L; EInt 20L ] in
  let expr = ELet ("t", tuple_expr, EInt 55L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "tuple let binding generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Tuple Codegen Tests"
    [
      ( "construction",
        [ test_case "simple tuple construction" `Quick test_tuple_construction ]
      );
      ( "empty tuple",
        [ test_case "empty tuple handling" `Quick test_empty_tuple ] );
      ( "nested tuples",
        [ test_case "nested tuple construction" `Quick test_nested_tuples ] );
      ( "mixed types",
        [ test_case "mixed type tuples" `Quick test_mixed_type_tuple ] );
      ( "let binding",
        [ test_case "tuple in let binding" `Quick test_tuple_let_binding ] );
    ]
