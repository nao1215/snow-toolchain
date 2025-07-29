open Alcotest
open Ast
open Codegen

(** Test unknown variable error *)
let test_unknown_variable_error () =
  (* Test: reference to undefined variable *)
  let expr = EVar "undefined_variable" in

  try
    let _ = generate_llvm_ir expr in
    fail "Expected Codegen_error for unknown variable"
  with
  | Codegen_error msg ->
      check bool "Error message mentions variable" true
        (String.contains msg 'v' && String.contains msg 'a')
  | exn ->
      fail
        (Printf.sprintf "Expected Codegen_error, got: %s"
           (Printexc.to_string exn))

(** Test unknown function error *)
let test_unknown_function_error () =
  (* Test: call to undefined function *)
  let expr = ECall (EVar "undefined_function", [ EInt 42L ]) in

  try
    let _ = generate_llvm_ir expr in
    fail "Expected Codegen_error for unknown function"
  with
  | Codegen_error msg ->
      check bool "Error message mentions function" true
        (String.contains msg 'f' && String.contains msg 'u')
  | exn ->
      fail
        (Printf.sprintf "Expected Codegen_error, got: %s"
           (Printexc.to_string exn))

(** Test non-literal in literal context error *)
let test_non_literal_error () =
  (* This tests the codegen_literal function directly with a non-literal *)
  let ctx = create_context "test_module" in

  try
    let _ = codegen_literal ctx (EVar "not_a_literal") in
    fail "Expected Codegen_error for non-literal"
  with
  | Codegen_error msg ->
      check bool "Error message mentions literal" true
        (String.contains msg 'l' && String.contains msg 'i')
  | exn ->
      fail
        (Printf.sprintf "Expected Codegen_error, got: %s"
           (Printexc.to_string exn))

(** Test complex expression with error *)
let test_complex_expression_error () =
  (* Test: complex expression with undefined variable in the middle *)
  let expr =
    ELet ("x", EInt 10L, EBinOp (Add, EVar "x", EVar "undefined_var"))
  in

  try
    let _ = generate_llvm_ir expr in
    fail "Expected Codegen_error for undefined variable in complex expression"
  with
  | Codegen_error msg ->
      check bool "Error propagated from complex expression" true
        (String.length msg > 0)
  | exn ->
      fail
        (Printf.sprintf "Expected Codegen_error, got: %s"
           (Printexc.to_string exn))

(** Test function call with non-function expression (should be caught) *)
let test_invalid_function_call () =
  (* Test: attempt to call a non-function expression *)
  let expr = ECall (EInt 42L, [ EInt 1L ]) in

  try
    let _ = generate_llvm_ir expr in
    fail "Expected Codegen_error for non-function call"
  with
  | Codegen_error msg ->
      check bool "Error message indicates function call issue" true
        (String.length msg > 0)
  | exn ->
      fail
        (Printf.sprintf "Expected Codegen_error, got: %s"
           (Printexc.to_string exn))

(** Test graceful handling of valid expressions *)
let test_valid_expression_no_error () =
  (* Test: ensure valid expressions don't throw errors *)
  let expr = ELet ("x", EInt 42L, EBinOp (Add, EVar "x", EInt 8L)) in

  try
    let ir = generate_llvm_ir expr in
    check bool "Valid expression generates IR without error" true
      (String.length ir > 0)
  with exn ->
    fail
      (Printf.sprintf "Valid expression should not error: %s"
         (Printexc.to_string exn))

let () =
  run "Error Handling Codegen Tests"
    [
      ( "unknown variable",
        [
          test_case "undefined variable reference" `Quick
            test_unknown_variable_error;
        ] );
      ( "unknown function",
        [
          test_case "undefined function call" `Quick test_unknown_function_error;
        ] );
      ( "invalid literal",
        [
          test_case "non-literal in literal context" `Quick
            test_non_literal_error;
        ] );
      ( "complex expression error",
        [
          test_case "error in complex expression" `Quick
            test_complex_expression_error;
        ] );
      ( "invalid function call",
        [
          test_case "call non-function expression" `Quick
            test_invalid_function_call;
        ] );
      ( "valid expression",
        [
          test_case "valid expression succeeds" `Quick
            test_valid_expression_no_error;
        ] );
    ]
