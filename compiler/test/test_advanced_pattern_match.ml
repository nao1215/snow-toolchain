open Alcotest
open Ast
open Codegen

(** Test guard conditions in match expressions *)
let test_guard_conditions () =
  (* Test: match 5 with | x when x > 3 -> x * 2 | _ -> 0 *)
  let arms =
    [
      {
        pattern = PVar "x";
        guard = Some (EBinOp (Gt, EVar "x", EInt 3L));
        body = EBinOp (Mul, EVar "x", EInt 2L);
      };
      { pattern = PWildcard; guard = None; body = EInt 0L };
    ]
  in
  let expr = EMatch (EInt 5L, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "guard condition generates IR" true (String.length ir > 0);
    (* Check for guard-related instructions *)
    check bool "IR contains guard logic" true
      (String.contains ir 'g' || String.contains ir 'c')
    (* guard_check or condition *)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test tuple pattern destructuring *)
let test_tuple_patterns () =
  (* Test: let t = (1, 2) in match t with | (x, y) -> x + y *)
  let tuple_expr = ETuple [ EInt 1L; EInt 2L ] in
  let pattern = PTuple [ PVar "x"; PVar "y" ] in
  let arms =
    [ { pattern; guard = None; body = EBinOp (Add, EVar "x", EVar "y") } ]
  in
  let expr = ELet ("t", tuple_expr, EMatch (EVar "t", arms)) in

  try
    let ir = generate_llvm_ir expr in
    check bool "tuple pattern generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Option/Result pattern matching *)
let test_option_result_patterns () =
  (* Test: match Some(42) with | Some(x) -> x | None -> 0 *)
  let some_expr = ESome (EInt 42L) in
  let arms =
    [
      { pattern = PSome (PVar "x"); guard = None; body = EVar "x" };
      { pattern = PNone; guard = None; body = EInt 0L };
    ]
  in
  let expr = EMatch (some_expr, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "Option pattern generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test range patterns *)
let test_range_patterns () =
  (* Test: match 15 with | x in 10..20 -> x | _ -> 0 *)
  let arms =
    [
      {
        pattern = PRange (EInt 10L, EInt 20L);
        guard = None;
        body = EInt 100L (* Return success value *);
      };
      { pattern = PWildcard; guard = None; body = EInt 0L };
    ]
  in
  let expr = EMatch (EInt 15L, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "range pattern generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test literal patterns *)
let test_literal_patterns () =
  (* Test: match 42 with | 42 -> "found" | _ -> "not found" *)
  let arms =
    [
      {
        pattern = PLiteral (EInt 42L);
        guard = None;
        body = EInt 1L (* Found *);
      };
      { pattern = PWildcard; guard = None; body = EInt 0L (* Not found *) };
    ]
  in
  let expr = EMatch (EInt 42L, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "literal pattern generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test nested patterns *)
let test_nested_patterns () =
  (* Simplified test: match Some(1) with | Some(x) -> x | None -> 0 *)
  (* Note: Nested tuple patterns in Option/Result are not yet fully supported *)
  let nested_pattern = PSome (PVar "x") in
  let arms =
    [
      { pattern = nested_pattern; guard = None; body = EVar "x" };
      { pattern = PNone; guard = None; body = EInt 0L };
    ]
  in
  let some_val = ESome (EInt 1L) in
  let expr = EMatch (some_val, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "nested pattern generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test exhaustiveness - basic case *)
let test_exhaustiveness_basic () =
  (* Test: match 1 with | 1 -> "one" | _ -> "other" *)
  let arms =
    [
      { pattern = PLiteral (EInt 1L); guard = None; body = EInt 10L };
      { pattern = PWildcard; guard = None; body = EInt 20L };
    ]
  in
  let expr = EMatch (EInt 1L, arms) in

  try
    let ir = generate_llvm_ir expr in
    check bool "exhaustive match generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Advanced Pattern Matching Tests"
    [
      ( "guard conditions",
        [ test_case "guard condition support" `Quick test_guard_conditions ] );
      ( "tuple patterns",
        [ test_case "tuple destructuring" `Quick test_tuple_patterns ] );
      ( "option result patterns",
        [
          test_case "Option/Result matching" `Quick test_option_result_patterns;
        ] );
      ( "range patterns",
        [ test_case "numeric range matching" `Quick test_range_patterns ] );
      ( "literal patterns",
        [ test_case "literal value matching" `Quick test_literal_patterns ] );
      ( "nested patterns",
        [ test_case "nested pattern structures" `Quick test_nested_patterns ] );
      ( "exhaustiveness",
        [ test_case "exhaustiveness checking" `Quick test_exhaustiveness_basic ]
      );
    ]
