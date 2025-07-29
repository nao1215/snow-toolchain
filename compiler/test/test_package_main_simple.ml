(*** @project snow-toolchain @module Test_package_main_simple

  Simple test for package main functionality *)

open Snow
open Ast

(** Simple test that validates basic package main functionality *)
let test_basic_functionality () =
  Printf.printf "Testing basic package main functionality...\n";
  let main_func =
    {
      fname = "main";
      params = [];
      ret = TUnit;
      body = [ SExpr EUnit ];
      is_extern = false;
    }
  in
  let program =
    {
      package_name = "main";
      imports = [];
      type_defs = [];
      functions = [ main_func ];
    }
  in
  try
    let llvm_ir = Codegen.generate_llvm_ir_with_package program in
    if String.length llvm_ir > 0 then (
      Printf.printf "✓ Basic package main test passed\n";
      Printf.printf "Generated IR length: %d characters\n"
        (String.length llvm_ir);
      true)
    else (
      Printf.printf "✗ Basic package main test failed: empty IR\n";
      false)
  with exn ->
    Printf.printf "✗ Basic package main test failed: %s\n"
      (Printexc.to_string exn);
    false

(** Run the basic test *)
let run_tests () =
  Printf.printf "\n=== Running Simple Package Main Test ===\n";
  let success = test_basic_functionality () in
  if success then Printf.printf "✓ Implementation working correctly.\n"
  else Printf.printf "✗ Test failed.\n";
  Printf.printf "=== Simple Package Main Test Complete ===\n\n"

let () = run_tests ()
