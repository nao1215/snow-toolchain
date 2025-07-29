open Alcotest
open Ast
open Codegen

(** Helper to check if string contains substring *)
let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(** Test simple arithmetic computation *)
let test_simple_computation () =
  (* Simple computation: 10 + 20 *)
  let expr = EBinOp (Add, EInt 10L, EInt 20L) in

  (* Test that computation generates valid LLVM IR *)
  try
    let ir = generate_llvm_ir expr in
    (* Check that IR contains computation result (LLVM optimizes 10 + 20 to
       30) *)
    check bool "computation generates IR" true (String.length ir > 0);
    check bool "IR contains result" true (contains_substring ir "30")
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test simple function call *)
let test_function_call () =
  (* let f = fun(x: int) -> int = x * 2 in f(5) *)
  let expr =
    ELet
      ( "f",
        EFun
          ( [ { name = "x"; param_type = TInt } ],
            TInt,
            EBinOp (Mul, EVar "x", EInt 2L) ),
        ECall (EVar "f", [ EInt 5L ]) )
  in

  (* Test that function call generates valid LLVM IR *)
  try
    let ir = generate_llvm_ir expr in
    check bool "function call generates IR" true (String.length ir > 0);
    check bool "IR contains call instruction" true
      (contains_substring ir "call")
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Function Codegen Tests"
    [
      ( "simple computation",
        [ test_case "simple arithmetic" `Quick test_simple_computation ] );
      ( "function call",
        [ test_case "simple function call" `Quick test_function_call ] );
    ]
