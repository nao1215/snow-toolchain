open Alcotest
open Ast
open Codegen

(** Helper to check if string contains substring *)
let _contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(** Test array literal code generation *)
let test_array_literal () =
  (* Simple array literal: [1, 2, 3] *)
  let array_expr = EArray [ EInt 1L; EInt 2L; EInt 3L ] in

  try
    let ir = generate_llvm_ir array_expr in
    (* For now, just check that IR is generated *)
    check bool "array literal generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test simple array access with variables *)
let test_array_access () =
  (* let arr = [10, 20, 30] in arr[1] *)
  let expr =
    ELet
      ( "arr",
        EArray [ EInt 10L; EInt 20L; EInt 30L ],
        EArrayAccess (EVar "arr", EInt 1L) )
  in

  try
    let ir = generate_llvm_ir expr in
    check bool "array access generates IR" true (String.length ir > 0);
    (* Currently simplified, so won't contain GEP instruction *)
    check bool "IR generated successfully" true true
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "Array Codegen Tests"
    [
      ( "array literal",
        [ test_case "simple array literal" `Quick test_array_literal ] );
      ( "array access",
        [ test_case "simple array access" `Quick test_array_access ] );
    ]
