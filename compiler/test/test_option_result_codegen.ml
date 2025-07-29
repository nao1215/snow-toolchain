open Alcotest
open Ast
open Codegen

(** Test Option Some construction *)
let test_some_construction () =
  (* Test: Some(42) - these are already i64 compatible, so direct test is
     safe *)
  let expr = ESome (EInt 42L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "Some construction generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Option None construction *)
let test_none_construction () =
  (* Test: None - returns i64, so direct test is safe *)
  let expr = ENone in

  try
    let ir = generate_llvm_ir expr in
    check bool "None construction generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Result Ok construction *)
let test_ok_construction () =
  (* Test: Ok(100) - returns i64, so direct test is safe *)
  let expr = EOk (EInt 100L) in

  try
    let ir = generate_llvm_ir expr in
    check bool "Ok construction generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Result Err construction *)
let test_err_construction () =
  (* Test: Err("error message") - returns i64, so direct test is safe *)
  let expr = EErr (EString "error message") in

  try
    let ir = generate_llvm_ir expr in
    check bool "Err construction generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Option in let binding *)
let test_option_let_binding () =
  (* Test: let opt = Some(5) in opt *)
  let some_expr = ESome (EInt 5L) in
  let expr = ELet ("opt", some_expr, EVar "opt") in

  try
    let ir = generate_llvm_ir expr in
    check bool "Option let binding generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test Result in let binding *)
let test_result_let_binding () =
  (* Test: let res = Ok(42) in res *)
  let ok_expr = EOk (EInt 42L) in
  let expr = ELet ("res", ok_expr, EVar "res") in

  try
    let ir = generate_llvm_ir expr in
    check bool "Result let binding generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test nested Option/Result *)
let test_nested_option_result () =
  (* Test: Some(Ok(123)) *)
  let ok_expr = EOk (EInt 123L) in
  let expr = ESome ok_expr in

  try
    let ir = generate_llvm_ir expr in
    check bool "Nested Option/Result generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Option/Result Codegen Tests"
    [
      ( "Option Some",
        [ test_case "Some construction" `Quick test_some_construction ] );
      ( "Option None",
        [ test_case "None construction" `Quick test_none_construction ] );
      ("Result Ok", [ test_case "Ok construction" `Quick test_ok_construction ]);
      ( "Result Err",
        [ test_case "Err construction" `Quick test_err_construction ] );
      ( "Option binding",
        [ test_case "Option in let binding" `Quick test_option_let_binding ] );
      ( "Result binding",
        [ test_case "Result in let binding" `Quick test_result_let_binding ] );
      ( "nested constructs",
        [ test_case "nested Option/Result" `Quick test_nested_option_result ] );
    ]
