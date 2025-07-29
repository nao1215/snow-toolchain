module A = Alcotest
open Snow.Lexer
open Snow.Parser
open Snow.Semantic

(** Helper to parse and semantically analyze a string *)
let check_semantic input =
  try
    let tokens = lex input in
    match parse tokens with
    | Ok expr -> semantic_check expr
    | Error msg -> Error ("Parse error: " ^ msg)
  with Lex_error msg -> Error ("Lex error: " ^ msg)

(** Test variable scope *)
let test_var_scope () =
  let valid_cases =
    [
      "42";
      (* Literal - no variables *)
      "let x = 42 in x";
      (* Simple let binding *)
      "let x = 1 in let y = 2 in x + y";
      (* Nested let *)
    ]
  in
  List.iter
    (fun input ->
      match check_semantic input with
      | Ok () -> () (* Success expected *)
      | Error msg ->
          A.fail
            (Printf.sprintf "Expected success for %s but got: %s" input msg))
    valid_cases

(** Test undefined variables *)
let test_undefined_vars () =
  let error_cases =
    [
      ("x", "Undefined variable");
      ("let x = 1 in y", "Undefined variable");
      ("x + y", "Undefined variable");
    ]
  in
  List.iter
    (fun (input, expected_error) ->
      match check_semantic input with
      | Ok () -> A.fail (Printf.sprintf "Expected error for %s" input)
      | Error msg ->
          if not (String.exists (fun c -> String.contains expected_error c) msg)
          then
            A.fail
              (Printf.sprintf "Expected error containing '%s' but got: %s"
                 expected_error msg))
    error_cases

(** Test naming conventions *)
let test_naming () =
  let valid_cases =
    [
      "let x = 1 in x"; "let myVar = 2 in myVar"; "let _private = 3 in _private";
    ]
  in
  List.iter
    (fun input ->
      match check_semantic input with
      | Ok () -> () (* Success expected *)
      | Error msg ->
          A.fail
            (Printf.sprintf "Expected success for %s but got: %s" input msg))
    valid_cases;

  let error_cases =
    [
      ("let X = 1 in X", "should start with lowercase");
      ("let MyVar = 2 in MyVar", "should start with lowercase");
    ]
  in
  List.iter
    (fun (input, expected_error) ->
      match check_semantic input with
      | Ok () -> A.fail (Printf.sprintf "Expected naming error for %s" input)
      | Error msg ->
          if not (String.exists (fun c -> String.contains expected_error c) msg)
          then
            A.fail
              (Printf.sprintf "Expected error containing '%s' but got: %s"
                 expected_error msg))
    error_cases

(** Test nested scopes *)
let test_nested_scopes () =
  let cases =
    [
      "let x = 1 in let x = 2 in x";
      (* Shadowing allowed *)
      "let x = 1 in (let y = 2 in y) + x";
      (* Inner scope *)
    ]
  in
  List.iter
    (fun input ->
      match check_semantic input with
      | Ok () -> () (* Success expected *)
      | Error msg ->
          A.fail
            (Printf.sprintf "Expected success for %s but got: %s" input msg))
    cases

(** Test advanced type constructs *)
let test_advanced_types () =
  let cases =
    [
      "(1, true, \"hello\")";
      (* Tuple *)
      "[1, 2, 3]";
      (* Array *)
      "Some(42)";
      (* Option Some *)
      "None";
      (* Option None *)
      "Ok(42)";
      (* Result Ok *)
      "Err(\"error\")";
      (* Result Err *)
      "let x = Some(1) in x";
      (* Option in let binding *)
      "let arr = [1, 2] in arr";
      (* Array in let binding *)
    ]
  in
  List.iter
    (fun input ->
      match check_semantic input with
      | Ok () -> () (* Success expected *)
      | Error msg ->
          A.fail (Printf.sprintf "Failed semantic check for %s: %s" input msg))
    cases

let () =
  A.run "Semantic Analysis Tests"
    [
      ( "semantic",
        [
          A.test_case "variable scope" `Quick test_var_scope;
          A.test_case "undefined variables" `Quick test_undefined_vars;
          A.test_case "naming conventions" `Quick test_naming;
          A.test_case "nested scopes" `Quick test_nested_scopes;
          A.test_case "advanced types" `Quick test_advanced_types;
        ] );
    ]
