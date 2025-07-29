module A = Alcotest
open Snow.Lexer
open Snow.Parser
open Snow.Typechecker

(** Helper to parse and type check a string *)
let check_type input =
  try
    let tokens = lex input in
    match parse tokens with
    | Ok expr -> type_check expr
    | Error msg -> Error ("Parse error: " ^ msg)
  with Lex_error msg -> Error ("Lex error: " ^ msg)

(** Test literal type inference *)
let test_literals () =
  let cases =
    [
      ("42", "int");
      ("42i32", "i32");
      ("255u8", "u8");
      ("3.14", "float");
      ("3.14f32", "f32");
      ("true", "bool");
      ("false", "bool");
      ("'A'", "char");
      ("\"hello\"", "string");
      ("()", "unit");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test binary operators *)
let test_binops () =
  let cases =
    [
      ("1 + 2", "int");
      ("1.0 + 2.0", "float");
      ("\"hello\" + \" world\"", "string");
      ("5 - 3", "int");
      ("10 * 2", "int");
      ("8 / 2", "int");
      ("7 % 3", "int");
      ("1 == 2", "bool");
      ("1 != 2", "bool");
      ("1 < 2", "bool");
      ("true && false", "bool");
      ("true || false", "bool");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test let expressions *)
let test_let () =
  let cases =
    [
      ("let x = 42 in x", "int");
      ("let x = true in x", "bool");
      ("let x = 1 in let y = 2 in x + y", "int");
      ("let x = \"hello\" in x", "string");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test if expressions *)
let test_if_expr () =
  let cases =
    [
      ("if true then 1 else 2", "int");
      ("if false then \"yes\" else \"no\"", "string");
      ("if 1 < 2 then 3.14 else 2.71", "float");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test Option and Result types *)
let test_option_result () =
  let cases =
    [
      ("Some(42)", "Option<int>");
      ("Some(true)", "Option<bool>");
      ("Some(\"hello\")", "Option<string>");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test tuples *)
let test_tuples () =
  let cases =
    [
      ("(42, true)", "(int, bool)");
      ("(1, 2, 3)", "(int, int, int)");
      ("(\"hello\", 3.14, false)", "(string, float, bool)");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test arrays *)
let test_arrays () =
  let cases =
    [
      ("[1, 2, 3]", "[int]");
      ("[true, false]", "[bool]");
      ("[\"a\", \"b\", \"c\"]", "[string]");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test pattern matching *)
let test_pattern_matching () =
  let cases =
    [
      ("match Some(42) with | Some(x) -> x | None -> 0", "int");
      ("match (1, true) with | (x, y) -> x", "int");
      ("match true with | true -> 1 | false -> 0", "int");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test functions *)
let test_functions () =
  let cases =
    [
      ("fun(x: int) -> int = x + 1", "(int) -> int");
      ("fun(x: int, y: int) -> int = x + y", "(int, int) -> int");
      ("let add = fun(x: int, y: int) -> int = x + y in add(1, 2)", "int");
      ("let identity = fun(x: int) -> int = x in identity(42)", "int");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test control structures *)
let test_control_structures () =
  let cases =
    [
      ("{ 1; 2; 3 }", "int");
      (* Block expression *)
      ("{}", "unit");
      (* Empty block *)
      ("while true do 42", "unit");
      (* While loop *)
      ("for x in [1, 2, 3] do x + 1", "unit");
      (* For loop *)
      ("let arr = [1, 2] in { let sum = 0 in for x in arr do sum + x }", "unit");
    ]
  in
  List.iter
    (fun (input, expected) ->
      match check_type input with
      | Ok typ ->
          let actual = Snow.Ast.TypeUtils.typ_to_string typ in
          A.check A.string input expected actual
      | Error msg ->
          A.fail (Printf.sprintf "Failed to type check %s: %s" input msg))
    cases

(** Test type errors *)
let test_type_errors () =
  let cases =
    [
      ("1 + true", "Type error");
      ("\"hello\" - \"world\"", "Type error");
      ("x", "Undefined variable");
      ("let x = 1 in y", "Undefined variable");
      ("if 42 then 1 else 2", "If condition must be bool");
      ("if true then 1 else \"no\"", "If branches have different types");
      ("[1, true]", "Array elements must have same type");
      ("[]", "Empty array requires type annotation");
      ("None", "None requires type annotation");
      ( "match Some(1) with | Some(x) -> \"string\" | None -> 42",
        "Match arms have different types" );
      ("fun(x: int) -> string = x + 1", "Function body type");
      ("let f = fun(x: int) -> int = x in f(true)", "Argument type");
      ("let f = fun(x: int) -> int = x in f(1, 2)", "Function expects");
      ("while 42 do true", "While condition must be bool");
      ("for x in 42 do x", "For loop requires array type");
    ]
  in
  List.iter
    (fun (input, expected_error) ->
      match check_type input with
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
    cases

let () =
  A.run "Typechecker Tests"
    [
      ( "type_checking",
        [
          A.test_case "literals" `Quick test_literals;
          A.test_case "binary operators" `Quick test_binops;
          A.test_case "let expressions" `Quick test_let;
          A.test_case "if expressions" `Quick test_if_expr;
          A.test_case "option result types" `Quick test_option_result;
          A.test_case "tuples" `Quick test_tuples;
          A.test_case "arrays" `Quick test_arrays;
          A.test_case "pattern matching" `Quick test_pattern_matching;
          A.test_case "functions" `Quick test_functions;
          A.test_case "control structures" `Quick test_control_structures;
          A.test_case "type errors" `Quick test_type_errors;
        ] );
    ]
