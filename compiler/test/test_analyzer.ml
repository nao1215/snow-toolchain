module A = Alcotest
open Snow.Lexer
open Snow.Parser
open Snow.Analyzer

(** Helper to parse and analyze a string *)
let analyze_string input =
  try
    let tokens = lex input in
    match parse tokens with
    | Ok expr -> analyze_type expr
    | Error msg -> Error ("Parse error: " ^ msg)
  with Lex_error msg -> Error ("Lex error: " ^ msg)

(** Test combined analysis *)
let test_combined () =
  let valid_cases =
    [
      ("42", "int");
      ("let x = 42 in x", "int");
      ("let x = 1 in let y = 2 in x + y", "int");
      ("if true then 1 else 2", "int");
      ("let myVar = \"hello\" in myVar", "string");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match analyze_string input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Analysis failed for %s: %s" input msg))
    valid_cases

(** Test combined errors *)
let test_errors () =
  let error_cases =
    [
      ("x", "Undefined variable");
      (* Semantic error *)
      ("let X = 1 in X", "should start with lowercase");
      (* Naming error *)
      ("1 + true", "Type error");
      (* Type error *)
      ("if 42 then 1 else 2", "If condition must be bool");
      (* Type error *)
    ]
  in
  List.iter
    (fun (input, expected_error) ->
      match analyze_string input with
      | Ok typ ->
          A.fail
            (Printf.sprintf "Expected error for %s but got type %s" input
               (Snow.Ast.TypeUtils.typ_to_string typ))
      | Error msg ->
          if not (String.exists (fun c -> String.contains expected_error c) msg)
          then
            A.fail
              (Printf.sprintf "Expected error containing '%s' but got: %s"
                 expected_error msg))
    error_cases

let () =
  A.run "Analyzer Tests"
    [
      ( "analyzer",
        [
          A.test_case "combined analysis" `Quick test_combined;
          A.test_case "error detection" `Quick test_errors;
        ] );
    ]
