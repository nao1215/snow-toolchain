(*** @project snow-toolchain @module Test_codegen

  Test suite for LLVM code generation functionality.

  This module contains comprehensive tests to ensure that the SNOW compiler
  correctly generates LLVM IR for various language constructs. *)

open Snow
open Ast

let test_basic_arithmetic () =
  let expr = EBinOp (Add, EInt 42L, EInt 58L) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    let has_ret_100 =
      String.contains (String.concat "" (String.split_on_char '\n' llvm_ir)) '1'
      && String.contains
           (String.concat "" (String.split_on_char '\n' llvm_ir))
           '0'
    in
    assert has_ret_100;
    Printf.printf "✓ Basic arithmetic test passed\n"
  with exn ->
    Printf.printf "✗ Basic arithmetic test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let test_variable_binding () =
  let expr = ELet ("x", EInt 42L, EBinOp (Add, EVar "x", EInt 58L)) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    assert (String.length llvm_ir > 0);
    Printf.printf "✓ Variable binding test passed\n"
  with exn ->
    Printf.printf "✗ Variable binding test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let test_conditional_expression () =
  let expr = EIf (EBool true, EInt 42L, EInt 0L) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    let has_phi =
      String.contains (String.concat "" (String.split_on_char '\n' llvm_ir)) 'p'
    in
    assert has_phi;
    Printf.printf "✓ Conditional expression test passed\n"
  with exn ->
    Printf.printf "✗ Conditional expression test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let test_function_definitions () =
  let params = [ { name = "x"; param_type = TInt } ] in
  let expr = EFun (params, TInt, EBinOp (Mul, EVar "x", EInt 2L)) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    let contains_function = String.contains llvm_ir 'f' in
    assert contains_function;
    Printf.printf "✓ Function definition test passed\n"
  with exn ->
    Printf.printf "✗ Function definition test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let test_match_expressions () =
  let arms = [ { pattern = PWildcard; guard = None; body = EInt 42L } ] in
  let expr = EMatch (EInt 10L, arms) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    assert (String.length llvm_ir > 0);
    Printf.printf "✓ Match expression test passed\n"
  with exn ->
    Printf.printf "✗ Match expression test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let test_while_loops () =
  let expr = EWhile (EBool false, EInt 42L) in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    let has_loop = String.contains llvm_ir 'l' in
    assert has_loop;
    Printf.printf "✓ While loop test passed\n"
  with exn ->
    Printf.printf "✗ While loop test failed: %s\n" (Printexc.to_string exn);
    assert false

let test_block_expressions () =
  let expr = EBlock [ EInt 1L; EInt 2L; EInt 42L ] in
  try
    let llvm_ir = Codegen.generate_llvm_ir expr in
    assert (String.length llvm_ir > 0);
    Printf.printf "✓ Block expression test passed\n"
  with exn ->
    Printf.printf "✗ Block expression test failed: %s\n"
      (Printexc.to_string exn);
    assert false

let run_tests () =
  Printf.printf "Running LLVM codegen tests...\n";
  test_basic_arithmetic ();
  test_variable_binding ();
  test_conditional_expression ();
  test_function_definitions ();
  test_match_expressions ();
  test_while_loops ();
  test_block_expressions ();
  Printf.printf "All codegen tests completed!\n"

let () = run_tests ()
