open Alcotest
open Ast
open Codegen

(** Test basic for loop code generation *)
let test_for_loop () =
  (* Simple for loop: for x in [10, 20] do x *)
  let array_expr = EArray [ EInt 10L; EInt 20L ] in
  let for_expr = EFor ("x", array_expr, EVar "x") in

  try
    let ir = generate_llvm_ir for_expr in
    (* Just check that IR is generated without errors *)
    check bool "for loop generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Test for loop with variable binding *)
let test_for_loop_binding () =
  (* For loop with let binding: let result = (for i in [5] do i + 1) in
     result *)
  let array_expr = EArray [ EInt 5L ] in
  let for_body = EBinOp (Add, EVar "i", EInt 1L) in
  let for_expr = EFor ("i", array_expr, for_body) in
  let let_expr = ELet ("result", for_expr, EVar "result") in

  try
    let ir = generate_llvm_ir let_expr in
    check bool "for loop with binding generates IR" true (String.length ir > 0)
  with
  | Codegen_error msg -> fail (Printf.sprintf "Codegen error: %s" msg)
  | exn -> fail (Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

let () =
  run "For Loop Codegen Tests"
    [
      ("basic for loop", [ test_case "simple for loop" `Quick test_for_loop ]);
      ( "for loop binding",
        [
          test_case "for loop with variable binding" `Quick
            test_for_loop_binding;
        ] );
    ]
