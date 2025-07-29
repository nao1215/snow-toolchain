open Lexer
open Parser
open Ast
module Test = Alcotest

(** Helper function to parse string input *)
let parse_string input =
  try
    let tokens = lex input in
    parse tokens
  with Lexer.Lex_error msg -> Error ("Lexer error: " ^ msg)

(** Convert AST expressions to string for testing *)
let rec expr_to_string = function
  | EI8 n -> Printf.sprintf "EI8(%d)" n
  | EU8 n -> Printf.sprintf "EU8(%d)" n
  | EI16 n -> Printf.sprintf "EI16(%d)" n
  | EU16 n -> Printf.sprintf "EU16(%d)" n
  | EI32 n -> Printf.sprintf "EI32(%d)" n
  | EU32 n -> Printf.sprintf "EU32(%d)" n
  | EI64 n -> Printf.sprintf "EI64(%Ld)" n
  | EU64 n -> Printf.sprintf "EU64(%Ld)" n
  | EInt n -> Printf.sprintf "EInt(%Ld)" n
  | EUint n -> Printf.sprintf "EUint(%Ld)" n
  | EF32 f -> Printf.sprintf "EF32(%g)" f
  | EF64 f -> Printf.sprintf "EF64(%g)" f
  | EFloat f -> Printf.sprintf "EFloat(%g)" f
  | EBool b -> Printf.sprintf "EBool(%b)" b
  | EChar c -> Printf.sprintf "EChar('%c')" c
  | EString s -> Printf.sprintf "EString(\"%s\")" s
  | EBytes b ->
      let hex_list =
        List.init (Bytes.length b) (fun i ->
            Printf.sprintf "0x%02X" (Bytes.get_uint8 b i))
      in
      Printf.sprintf "EBytes([%s])" (String.concat "; " hex_list)
  | EUnit -> "EUnit"
  | EVar s -> Printf.sprintf "EVar(%s)" s
  | ECall (func_expr, args) ->
      let args_str = String.concat "; " (List.map expr_to_string args) in
      Printf.sprintf "ECall(%s, [%s])" (expr_to_string func_expr) args_str
  | EArrayAccess (arr, index) ->
      Printf.sprintf "EArrayAccess(%s, %s)" (expr_to_string arr)
        (expr_to_string index)
  | EBinOp (op, l, r) ->
      Printf.sprintf "EBinOp(%s, %s, %s)" (binop_to_string op)
        (expr_to_string l) (expr_to_string r)
  | EUnOp (Not, e) -> Printf.sprintf "EUnOp(Not, %s)" (expr_to_string e)
  | EUnOp (Neg, e) -> Printf.sprintf "EUnOp(Neg, %s)" (expr_to_string e)
  | ESome e -> Printf.sprintf "ESome(%s)" (expr_to_string e)
  | ENone -> "ENone"
  | EOk e -> Printf.sprintf "EOk(%s)" (expr_to_string e)
  | EErr e -> Printf.sprintf "EErr(%s)" (expr_to_string e)
  | ETuple es ->
      Printf.sprintf "ETuple([%s])"
        (String.concat "; " (List.map expr_to_string es))
  | EIf (cond, then_expr, else_expr) ->
      Printf.sprintf "EIf(%s, %s, %s)" (expr_to_string cond)
        (expr_to_string then_expr) (expr_to_string else_expr)
  | EMatch (scrutinee, arms) ->
      let arms_str = String.concat "; " (List.map match_arm_to_string arms) in
      Printf.sprintf "EMatch(%s, [%s])" (expr_to_string scrutinee) arms_str
  | ELet (identifier, value_expr, body_expr) ->
      Printf.sprintf "ELet(%s, %s, %s)" identifier
        (expr_to_string value_expr)
        (expr_to_string body_expr)
  | EFun (params, return_type, body_expr) ->
      let params_str = String.concat "; " (List.map param_to_string params) in
      Printf.sprintf "EFun([%s], %s, %s)" params_str
        (type_to_string return_type)
        (expr_to_string body_expr)
  | EBlock exprs ->
      let exprs_str = String.concat "; " (List.map expr_to_string exprs) in
      Printf.sprintf "EBlock([%s])" exprs_str
  | EArray elements ->
      let elements_str =
        String.concat "; " (List.map expr_to_string elements)
      in
      Printf.sprintf "EArray([%s])" elements_str
  | EStruct fields ->
      let fields_str =
        String.concat "; " (List.map struct_field_init_to_string fields)
      in
      Printf.sprintf "EStruct([%s])" fields_str
  | EFieldAccess (expr, field) ->
      Printf.sprintf "EFieldAccess(%s, %s)" (expr_to_string expr) field
  | EWhile (condition, body) ->
      Printf.sprintf "EWhile(%s, %s)" (expr_to_string condition)
        (expr_to_string body)
  | EFor (var, iterable, body) ->
      Printf.sprintf "EFor(%s, %s, %s)" var (expr_to_string iterable)
        (expr_to_string body)
  | EPrint expr -> Printf.sprintf "EPrint(%s)" (expr_to_string expr)
  | EPrintln expr -> Printf.sprintf "EPrintln(%s)" (expr_to_string expr)
  | EUnsafe expr -> Printf.sprintf "EUnsafe(%s)" (expr_to_string expr)

and param_to_string param =
  Printf.sprintf "%s:%s" param.name (type_to_string param.param_type)

and struct_field_init_to_string field =
  Printf.sprintf "%s = %s" field.init_field_name
    (expr_to_string field.init_value)

and type_to_string = function
  | TVar id -> Printf.sprintf "'%d" id
  | TI8 -> "i8"
  | TU8 -> "u8"
  | TI16 -> "i16"
  | TU16 -> "u16"
  | TI32 -> "i32"
  | TU32 -> "u32"
  | TI64 -> "i64"
  | TU64 -> "u64"
  | TInt -> "int"
  | TUint -> "uint"
  | TF32 -> "f32"
  | TF64 -> "f64"
  | TFloat -> "float"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TBytes -> "bytes"
  | TUnit -> "unit"
  | TNever -> "never"
  | TTuple types ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map type_to_string types))
  | TArray (t, None) -> Printf.sprintf "[%s]" (type_to_string t)
  | TArray (t, Some n) -> Printf.sprintf "[%s; %d]" (type_to_string t) n
  | TOption t -> Printf.sprintf "Option<%s>" (type_to_string t)
  | TResult (ok_t, err_t) ->
      Printf.sprintf "Result<%s, %s>" (type_to_string ok_t)
        (type_to_string err_t)
  | TFunc (arg_types, ret_type) ->
      let args_str = String.concat ", " (List.map type_to_string arg_types) in
      Printf.sprintf "(%s) -> %s" args_str (type_to_string ret_type)
  | TStruct fields ->
      let field_str =
        String.concat ", "
          (List.map
             (fun f ->
               Printf.sprintf "%s: %s" f.field_name
                 (type_to_string f.field_type))
             fields)
      in
      Printf.sprintf "{ %s }" field_str
  | TUnion (name, variants) ->
      let variant_str =
        String.concat " | "
          (List.map
             (fun v ->
               match v.variant_data with
               | None -> v.variant_name
               | Some t ->
                   Printf.sprintf "%s(%s)" v.variant_name (type_to_string t))
             variants)
      in
      Printf.sprintf "%s = %s" name variant_str

and match_arm_to_string arm =
  Printf.sprintf "{pattern=%s; body=%s}"
    (pattern_to_string arm.pattern)
    (expr_to_string arm.body)

and pattern_to_string = function
  | PWildcard -> "PWildcard"
  | PVar name -> Printf.sprintf "PVar(%s)" name
  | PLiteral expr -> Printf.sprintf "PLiteral(%s)" (expr_to_string expr)
  | PSome pattern -> Printf.sprintf "PSome(%s)" (pattern_to_string pattern)
  | PNone -> "PNone"
  | POk pattern -> Printf.sprintf "POk(%s)" (pattern_to_string pattern)
  | PErr pattern -> Printf.sprintf "PErr(%s)" (pattern_to_string pattern)
  | PTuple patterns ->
      Printf.sprintf "PTuple([%s])"
        (String.concat "; " (List.map pattern_to_string patterns))
  | PArray patterns ->
      Printf.sprintf "PArray([%s])"
        (String.concat "; " (List.map pattern_to_string patterns))
  | PRange (min_expr, max_expr) ->
      Printf.sprintf "PRange(%s..%s)" (expr_to_string min_expr)
        (expr_to_string max_expr)
  | PStruct field_patterns ->
      let field_strs =
        List.map
          (fun (name, pattern) ->
            Printf.sprintf "%s: %s" name (pattern_to_string pattern))
          field_patterns
      in
      Printf.sprintf "PStruct({%s})" (String.concat ", " field_strs)

and binop_to_string = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"
  | Assign -> "Assign"
  | Pipe -> "Pipe"

(** Test basic literal parsing *)
let test_literals () =
  let test_cases =
    [
      (* Basic literals *)
      ("integer", "42", "EInt(42)");
      ("boolean true", "true", "EBool(true)");
      ("boolean false", "false", "EBool(false)");
      ("character", "'A'", "EChar('A')");
      ("string", "\"hello\"", "EString(\"hello\")");
      ("unit", "()", "EUnit");
      (* All integer types with suffixes *)
      ("i8 literal", "127i8", "EI8(127)");
      ("u8 literal", "255u8", "EU8(255)");
      ("i16 literal", "32767i16", "EI16(32767)");
      ("u16 literal", "65535u16", "EU16(65535)");
      ("i32 literal", "42i32", "EI32(42)");
      ("u32 literal", "42u32", "EU32(42)");
      ("i64 literal", "9223372036854775807i64", "EI64(9223372036854775807)");
      ("u64 literal", "42u64", "EU64(42)");
      ("uint literal", "42uint", "EUint(42)");
      (* Float types *)
      ("float", "3.14", "EFloat(3.14)");
      ("f32 literal", "2.5f32", "EF32(2.5)");
      ("f64 literal", "3.14159f64", "EF64(3.14159)");
      (* Edge case literals *)
      ("zero", "0", "EInt(0)");
      ("escaped char", "'\\n'", "EChar('\n')");
      ("escaped string", "\"hello\\nworld\"", "EString(\"hello\nworld\")");
      ("empty string", "\"\"", "EString(\"\")");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test variable parsing *)
let test_variables () =
  let test_cases =
    [
      ("simple var", "x", "EVar(x)");
      ("camelCase var", "myVariable", "EVar(myVariable)");
      ("underscore var", "_private", "EVar(_private)");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test arithmetic expressions *)
let test_arithmetic () =
  let test_cases =
    [
      ("addition", "1 + 2", "EBinOp(Add, EInt(1), EInt(2))");
      ("subtraction", "5 - 3", "EBinOp(Sub, EInt(5), EInt(3))");
      ("multiplication", "4 * 6", "EBinOp(Mul, EInt(4), EInt(6))");
      ("division", "8 / 2", "EBinOp(Div, EInt(8), EInt(2))");
      ("modulo", "7 % 3", "EBinOp(Mod, EInt(7), EInt(3))");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test comparison expressions *)
let test_comparisons () =
  let test_cases =
    [
      ("equality", "a == b", "EBinOp(Eq, EVar(a), EVar(b))");
      ("inequality", "x != y", "EBinOp(Neq, EVar(x), EVar(y))");
      ("less than", "1 < 2", "EBinOp(Lt, EInt(1), EInt(2))");
      ("less equal", "3 <= 4", "EBinOp(Le, EInt(3), EInt(4))");
      ("greater than", "5 > 4", "EBinOp(Gt, EInt(5), EInt(4))");
      ("greater equal", "6 >= 5", "EBinOp(Ge, EInt(6), EInt(5))");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test logical expressions *)
let test_logical () =
  let test_cases =
    [
      ("logical and", "true && false", "EBinOp(And, EBool(true), EBool(false))");
      ("logical or", "false || true", "EBinOp(Or, EBool(false), EBool(true))");
      ("logical not", "!true", "EUnOp(Not, EBool(true))");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test operator precedence *)
let test_precedence () =
  let test_cases =
    [
      (* Basic precedence *)
      ( "mul before add",
        "1 + 2 * 3",
        "EBinOp(Add, EInt(1), EBinOp(Mul, EInt(2), EInt(3)))" );
      ( "div before sub",
        "10 - 8 / 2",
        "EBinOp(Sub, EInt(10), EBinOp(Div, EInt(8), EInt(2)))" );
      ( "mod with mul",
        "5 * 3 % 2",
        "EBinOp(Mod, EBinOp(Mul, EInt(5), EInt(3)), EInt(2))" );
      (* Parentheses override *)
      ( "parentheses",
        "(1 + 2) * 3",
        "EBinOp(Mul, EBinOp(Add, EInt(1), EInt(2)), EInt(3))" );
      ( "nested parentheses",
        "((1 + 2) * 3) / 4",
        "EBinOp(Div, EBinOp(Mul, EBinOp(Add, EInt(1), EInt(2)), EInt(3)), \
         EInt(4))" );
      (* Comparison precedence *)
      ( "comparison before logical",
        "1 < 2 && 3 > 4",
        "EBinOp(And, EBinOp(Lt, EInt(1), EInt(2)), EBinOp(Gt, EInt(3), \
         EInt(4)))" );
      ( "arithmetic before comparison",
        "1 + 2 < 3 * 4",
        "EBinOp(Lt, EBinOp(Add, EInt(1), EInt(2)), EBinOp(Mul, EInt(3), \
         EInt(4)))" );
      (* Logical operator precedence *)
      ( "and before or",
        "true || false && true",
        "EBinOp(Or, EBool(true), EBinOp(And, EBool(false), EBool(true)))" );
      ( "not highest precedence",
        "!true && false",
        "EBinOp(And, EUnOp(Not, EBool(true)), EBool(false))" );
      (* Complex mixed precedence *)
      ( "complex mixed",
        "1 + 2 * 3 == 7 && !false",
        "EBinOp(And, EBinOp(Eq, EBinOp(Add, EInt(1), EBinOp(Mul, EInt(2), \
         EInt(3))), EInt(7)), EUnOp(Not, EBool(false)))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test Option/Result constructors *)
let test_option_result () =
  let test_cases =
    [
      ("some", "Some(42)", "ESome(EInt(42))");
      ("none", "None", "ENone");
      ("ok", "Ok(\"success\")", "EOk(EString(\"success\"))");
      ("err", "Err(\"error\")", "EErr(EString(\"error\"))");
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test error cases *)
let test_errors () =
  let test_cases =
    [
      ("incomplete expression", "1 +");
      ("mismatched parens", "(1 + 2");
      ("invalid token", "@#$");
      ("if without then", "if true 1 else 2");
      ("if without else", "if true then 1");
      ("if then without else", "if true then");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> () (* Expected error *))
    test_cases

(** Test conditional expressions (if-then-else) *)
let test_conditionals () =
  let test_cases =
    [
      ( "simple if",
        "if true then 1 else 2",
        "EIf(EBool(true), EInt(1), EInt(2))" );
      ( "if with variables",
        "if x then y else z",
        "EIf(EVar(x), EVar(y), EVar(z))" );
      ( "nested if",
        "if true then if false then 1 else 2 else 3",
        "EIf(EBool(true), EIf(EBool(false), EInt(1), EInt(2)), EInt(3))" );
      ( "if with arithmetic",
        "if 1 < 2 then 3 + 4 else 5 * 6",
        "EIf(EBinOp(Lt, EInt(1), EInt(2)), EBinOp(Add, EInt(3), EInt(4)), \
         EBinOp(Mul, EInt(5), EInt(6)))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test tuple expressions *)
let test_tuples () =
  let test_cases =
    [
      ("pair", "(1, 2)", "ETuple([EInt(1); EInt(2)])");
      ( "triple",
        "(1, true, \"hello\")",
        "ETuple([EInt(1); EBool(true); EString(\"hello\")])" );
      ( "nested tuple",
        "((1, 2), 3)",
        "ETuple([ETuple([EInt(1); EInt(2)]); EInt(3)])" );
      ("single element", "(42)", "EInt(42)");
      (* Single element in parens is not a tuple *)
      ( "tuple with expressions",
        "(1 + 2, 3 * 4)",
        "ETuple([EBinOp(Add, EInt(1), EInt(2)); EBinOp(Mul, EInt(3), EInt(4))])"
      );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test complex nested expressions *)
let test_complex_expressions () =
  let test_cases =
    [
      (* Nested if expressions *)
      ( "if in condition",
        "if if true then false else true then 1 else 2",
        "EIf(EIf(EBool(true), EBool(false), EBool(true)), EInt(1), EInt(2))" );
      (* If with complex branches *)
      ( "if with operations",
        "if x > 0 then y + z else y - z",
        "EIf(EBinOp(Gt, EVar(x), EInt(0)), EBinOp(Add, EVar(y), EVar(z)), \
         EBinOp(Sub, EVar(y), EVar(z)))" );
      (* Option/Result in expressions *)
      ( "some in arithmetic",
        "Some(1 + 2)",
        "ESome(EBinOp(Add, EInt(1), EInt(2)))" );
      ("nested option/result", "Some(Ok(42))", "ESome(EOk(EInt(42)))");
      (* Mixed constructs *)
      ( "if with option",
        "if true then Some(x) else None",
        "EIf(EBool(true), ESome(EVar(x)), ENone)" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test match expressions *)
let test_match_expressions () =
  let test_cases =
    [
      (* Basic Option matching *)
      ( "simple option match",
        "match x with | Some(y) -> y | None -> 0",
        "EMatch(EVar(x), [{pattern=PSome(PVar(y)); body=EVar(y)}; \
         {pattern=PNone; body=EInt(0)}])" );
      (* Result matching *)
      ( "result match",
        "match result with | Ok(value) -> value + 1 | Err(msg) -> 0",
        "EMatch(EVar(result), [{pattern=POk(PVar(value)); body=EBinOp(Add, \
         EVar(value), EInt(1))}; {pattern=PErr(PVar(msg)); body=EInt(0)}])" );
      (* Literal patterns *)
      ( "literal match",
        "match n with | 0 -> \"zero\" | 1 -> \"one\" | _ -> \"other\"",
        "EMatch(EVar(n), [{pattern=PLiteral(EInt(0)); body=EString(\"zero\")}; \
         {pattern=PLiteral(EInt(1)); body=EString(\"one\")}; \
         {pattern=PWildcard; body=EString(\"other\")}])" );
      (* Boolean patterns *)
      ( "boolean match",
        "match flag with | true -> 1 | false -> 0",
        "EMatch(EVar(flag), [{pattern=PLiteral(EBool(true)); body=EInt(1)}; \
         {pattern=PLiteral(EBool(false)); body=EInt(0)}])" );
      (* Nested patterns *)
      ( "nested option match",
        "match opt with | Some(Some(x)) -> x | Some(None) -> 0 | None -> -1",
        "EMatch(EVar(opt), [{pattern=PSome(PSome(PVar(x))); body=EVar(x)}; \
         {pattern=PSome(PNone); body=EInt(0)}; {pattern=PNone; body=EUnOp(Neg, \
         EInt(1))}])" );
      (* Tuple patterns *)
      ( "tuple match",
        "match pair with | (0, y) -> y | (x, 0) -> x | (x, y) -> x + y",
        "EMatch(EVar(pair), [{pattern=PTuple([PLiteral(EInt(0)); PVar(y)]); \
         body=EVar(y)}; {pattern=PTuple([PVar(x); PLiteral(EInt(0))]); \
         body=EVar(x)}; {pattern=PTuple([PVar(x); PVar(y)]); body=EBinOp(Add, \
         EVar(x), EVar(y))}])" );
      (* Unit pattern *)
      ( "unit match",
        "match value with | () -> \"unit\"",
        "EMatch(EVar(value), [{pattern=PLiteral(EUnit); \
         body=EString(\"unit\")}])" );
      (* Pattern with negative number expression in body *)
      ( "negative number in body",
        "match n with | 0 -> -1 | 1 -> -2",
        "EMatch(EVar(n), [{pattern=PLiteral(EInt(0)); body=EUnOp(Neg, \
         EInt(1))}; {pattern=PLiteral(EInt(1)); body=EUnOp(Neg, EInt(2))}])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test let expressions *)
let test_let_expressions () =
  let test_cases =
    [
      (* Simple let binding *)
      ("simple let", "let x = 42 in x", "ELet(x, EInt(42), EVar(x))");
      (* Let with arithmetic *)
      ( "let with arithmetic",
        "let x = 1 + 2 in x * 3",
        "ELet(x, EBinOp(Add, EInt(1), EInt(2)), EBinOp(Mul, EVar(x), EInt(3)))"
      );
      (* Nested let expressions *)
      ( "nested let",
        "let x = 1 in let y = 2 in x + y",
        "ELet(x, EInt(1), ELet(y, EInt(2), EBinOp(Add, EVar(x), EVar(y))))" );
      (* Let with complex value *)
      ( "let with tuple",
        "let x = (1, 2) in x",
        "ELet(x, ETuple([EInt(1); EInt(2)]), EVar(x))" );
      (* Let with if-then-else in value *)
      ( "let with if value",
        "let x = if true then 1 else 2 in x + 3",
        "ELet(x, EIf(EBool(true), EInt(1), EInt(2)), EBinOp(Add, EVar(x), \
         EInt(3)))" );
      (* If-then-else with let in branches *)
      ( "if with let branches",
        "if true then let x = 1 in x else let y = 2 in y",
        "EIf(EBool(true), ELet(x, EInt(1), EVar(x)), ELet(y, EInt(2), EVar(y)))"
      );
      (* Let with match expression in value *)
      ( "let with match value",
        "let x = match Some(42) with | Some(n) -> n | None -> 0 in x",
        "ELet(x, EMatch(ESome(EInt(42)), [{pattern=PSome(PVar(n)); \
         body=EVar(n)}; {pattern=PNone; body=EInt(0)}]), EVar(x))" );
      (* Match with let in arms *)
      ( "match with let arms",
        "match Some(1) with | Some(n) -> let x = n + 1 in x | None -> 0",
        "EMatch(ESome(EInt(1)), [{pattern=PSome(PVar(n)); body=ELet(x, \
         EBinOp(Add, EVar(n), EInt(1)), EVar(x))}; {pattern=PNone; \
         body=EInt(0)}])" );
      (* Let with Option constructors *)
      ( "let with Some constructor",
        "let x = Some(42) in x",
        "ELet(x, ESome(EInt(42)), EVar(x))" );
      ( "let with Ok constructor",
        "let x = Ok(42) in x",
        "ELet(x, EOk(EInt(42)), EVar(x))" );
      (* Deep nesting of let expressions *)
      ( "deeply nested let",
        "let a = 1 in let b = 2 in let c = 3 in a + b + c",
        "ELet(a, EInt(1), ELet(b, EInt(2), ELet(c, EInt(3), EBinOp(Add, \
         EBinOp(Add, EVar(a), EVar(b)), EVar(c)))))" );
      (* Variable shadowing *)
      ( "variable shadowing",
        "let x = 1 in let x = 2 in x",
        "ELet(x, EInt(1), ELet(x, EInt(2), EVar(x)))" );
      (* Let with unary operators *)
      ( "let with unary not",
        "let x = !true in x",
        "ELet(x, EUnOp(Not, EBool(true)), EVar(x))" );
      ( "let with unary neg",
        "let x = -42 in x",
        "ELet(x, EUnOp(Neg, EInt(42)), EVar(x))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test let expression errors *)
let test_let_errors () =
  let test_cases =
    [
      (* Missing identifier *)
      ("let without identifier", "let = 42 in x");
      (* Missing assignment *)
      ("let without assignment", "let x 42 in x");
      (* Missing value *)
      ("let without value", "let x = in x");
      (* Missing in keyword *)
      ("let without in", "let x = 42 x");
      (* Missing body *)
      ("let without body", "let x = 42 in");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test let expressions with operator precedence *)
let test_let_precedence () =
  let test_cases =
    [
      (* Let expression precedence with addition *)
      ( "let precedence with addition",
        "let x = 1 + 2 in x * 3",
        "ELet(x, EBinOp(Add, EInt(1), EInt(2)), EBinOp(Mul, EVar(x), EInt(3)))"
      );
      (* Let with complex operator precedence *)
      ( "let with precedence",
        "let x = 1 + 2 * 3 in x && true",
        "ELet(x, EBinOp(Add, EInt(1), EBinOp(Mul, EInt(2), EInt(3))), \
         EBinOp(And, EVar(x), EBool(true)))" );
      (* Multiple let expressions with operators *)
      ( "multiple lets with operators",
        "let x = 1 in let y = 2 in x * y + 1",
        "ELet(x, EInt(1), ELet(y, EInt(2), EBinOp(Add, EBinOp(Mul, EVar(x), \
         EVar(y)), EInt(1))))" );
      (* Let in comparison expressions *)
      ( "let in comparison",
        "let x = 42 in x == 42",
        "ELet(x, EInt(42), EBinOp(Eq, EVar(x), EInt(42)))" );
      (* Let in logical expressions *)
      ( "let in logical expression",
        "let x = true in x || false",
        "ELet(x, EBool(true), EBinOp(Or, EVar(x), EBool(false)))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test let expressions with boundary cases *)
let test_let_boundary_cases () =
  let test_cases =
    [
      (* Single character identifier *)
      ("single char identifier", "let a = 1 in a", "ELet(a, EInt(1), EVar(a))");
      (* Underscore in identifier *)
      ( "underscore identifier",
        "let my_var = 42 in my_var",
        "ELet(my_var, EInt(42), EVar(my_var))" );
      (* Numbers in identifier *)
      ( "identifier with numbers",
        "let var123 = 456 in var123",
        "ELet(var123, EInt(456), EVar(var123))" );
      (* Different numeric types *)
      ( "let with different numeric types",
        "let x = 42i32 in let y = 3.14f64 in x",
        "ELet(x, EI32(42), ELet(y, EF64(3.14), EVar(x)))" );
      (* Let with unit value *)
      ("let with unit", "let x = () in x", "ELet(x, EUnit, EVar(x))");
      (* Let with character literal *)
      ("let with char", "let x = 'a' in x", "ELet(x, EChar('a'), EVar(x))");
      (* Let with string literal *)
      ( "let with string",
        "let x = \"hello\" in x",
        "ELet(x, EString(\"hello\"), EVar(x))" );
      (* Complex parenthesized expressions *)
      ( "let with parentheses",
        "let x = (1 + 2) * 3 in x",
        "ELet(x, EBinOp(Mul, EBinOp(Add, EInt(1), EInt(2)), EInt(3)), EVar(x))"
      );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test let expression edge cases and error conditions *)
let test_let_edge_cases () =
  let test_cases =
    [
      (* Keywords as identifiers should fail *)
      ("keyword as identifier", "let if = 42 in if");
      (* Reserved words *)
      ("reserved word let", "let let = 42 in let");
      ("reserved word in", "let in = 42 in in");
      (* Invalid expressions in value position *)
      ("incomplete value expression", "let x = 1 + in x");
      (* Invalid expressions in body position *)
      ("incomplete body expression", "let x = 1 in x +");
      (* Missing tokens *)
      ("incomplete nested let", "let x = let y = 1 in");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test function expressions *)
let test_function_expressions () =
  let test_cases =
    [
      (* Simple function with no parameters *)
      ("function no params", "fun () -> int = 42", "EFun([], int, EInt(42))");
      (* Function with single parameter *)
      ( "function single param",
        "fun(x: int) -> int = x + 1",
        "EFun([x:int], int, EBinOp(Add, EVar(x), EInt(1)))" );
      (* Function with multiple parameters *)
      ( "function multiple params",
        "fun(x: int, y: int) -> int = x + y",
        "EFun([x:int; y:int], int, EBinOp(Add, EVar(x), EVar(y)))" );
      (* Function with different types *)
      ( "function mixed types",
        "fun(name: string, age: i32) -> string = name",
        "EFun([name:string; age:i32], string, EVar(name))" );
      (* Function returning boolean *)
      ( "function returning bool",
        "fun(x: int, y: int) -> bool = x > y",
        "EFun([x:int; y:int], bool, EBinOp(Gt, EVar(x), EVar(y)))" );
      (* Function with complex body *)
      ( "function with if expression",
        "fun(x: int) -> int = if x > 0 then x else -x",
        "EFun([x:int], int, EIf(EBinOp(Gt, EVar(x), EInt(0)), EVar(x), \
         EUnOp(Neg, EVar(x))))" );
      (* Nested function in let *)
      ( "let with function",
        "let f = fun(x: int) -> int = x * 2 in f",
        "ELet(f, EFun([x:int], int, EBinOp(Mul, EVar(x), EInt(2))), EVar(f))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test function expression errors *)
let test_function_errors () =
  let test_cases =
    [
      (* Missing parentheses *)
      ("function without parens", "fun x: int -> int = x");
      (* Missing parameter type *)
      ("function missing param type", "fun(x) -> int = x");
      (* Missing colon in parameter *)
      ("function missing colon", "fun(x int) -> int = x");
      (* Missing arrow *)
      ("function missing arrow", "fun(x: int) int = x");
      (* Missing return type *)
      ("function missing return type", "fun(x: int) -> = x");
      (* Missing equals *)
      ("function missing equals", "fun(x: int) -> int x");
      (* Missing body *)
      ("function missing body", "fun(x: int) -> int =");
      (* Trailing comma in parameter list *)
      ("function trailing comma params", "fun(x: int,) -> int = x");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test match expression errors *)
let test_match_errors () =
  let test_cases =
    [
      (* Missing with keyword *)
      ("match without with", "match x | Some(y) -> y");
      (* Missing pipe for first arm *)
      ("match without pipe", "match x with Some(y) -> y");
      (* Missing arrow in arm *)
      ("match without arrow", "match x with | Some(y) y");
      (* Empty match arms *)
      ("match without arms", "match x with");
      (* Incomplete pattern *)
      ("incomplete pattern", "match x with | Some( -> 1");
      (* Missing pattern *)
      ("missing pattern", "match x with | -> 1");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> () (* Expected error *))
    test_cases

(** Test comprehensive error cases *)
let test_comprehensive_errors () =
  let test_cases =
    [
      (* Basic syntax errors *)
      ("incomplete expression", "1 +");
      ("mismatched parens", "(1 + 2");
      ("invalid token", "@#$");
      (* If expression errors *)
      ("if without then", "if true 1 else 2");
      ("if without else", "if true then 1");
      ("if then without else", "if true then");
      ("if without condition", "then 1 else 2");
      (* Option/Result errors *)
      ("some without parens", "Some 42");
      ("ok without parens", "Ok 42");
      ("err without parens", "Err 42");
      (* Tuple errors *)
      ("unclosed tuple", "(1, 2");
      ("tuple with trailing comma", "(1, 2,)");
      (* Operator errors *)
      ("double operator", "1 + + 2");
      ("operator without operand", "* 5");
      ("unbalanced operators", "1 + 2 *");
      (* Parentheses errors *)
      ("extra closing paren", "1 + 2)");
      ("nested unclosed", "((1 + 2)");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> () (* Expected error *))
    test_cases

(** Test function call expressions *)
let test_function_calls () =
  let test_cases =
    [
      (* Simple function call with no arguments *)
      ("function call no args", "foo()", "ECall(EVar(foo), [])");
      (* Function call with single argument *)
      ("function call single arg", "max(42)", "ECall(EVar(max), [EInt(42)])");
      (* Function call with multiple arguments *)
      ( "function call multiple args",
        "add(1, 2)",
        "ECall(EVar(add), [EInt(1); EInt(2)])" );
      (* Function call with different types *)
      ( "function call mixed types",
        "greet(\"hello\", 42)",
        "ECall(EVar(greet), [EString(\"hello\"); EInt(42)])" );
      (* Function call with complex expressions as arguments *)
      ( "function call with expressions",
        "calculate(1 + 2, 3 * 4)",
        "ECall(EVar(calculate), [EBinOp(Add, EInt(1), EInt(2)); EBinOp(Mul, \
         EInt(3), EInt(4))])" );
      (* Function call with variable arguments *)
      ( "function call with variables",
        "process(x, y, z)",
        "ECall(EVar(process), [EVar(x); EVar(y); EVar(z)])" );
      (* Nested function calls *)
      ( "nested function calls",
        "outer(inner(42))",
        "ECall(EVar(outer), [ECall(EVar(inner), [EInt(42)])])" );
      (* Function call with boolean arguments *)
      ( "function call with booleans",
        "check(true, false)",
        "ECall(EVar(check), [EBool(true); EBool(false)])" );
      (* Function call with Option/Result arguments *)
      ( "function call with Some",
        "handle(Some(42))",
        "ECall(EVar(handle), [ESome(EInt(42))])" );
      ("function call with None", "handle(None)", "ECall(EVar(handle), [ENone])");
      (* Function call in expressions *)
      ( "function call in arithmetic",
        "getValue() + 10",
        "EBinOp(Add, ECall(EVar(getValue), []), EInt(10))" );
      (* Function call with tuple argument *)
      ( "function call with tuple",
        "point((1, 2))",
        "ECall(EVar(point), [ETuple([EInt(1); EInt(2)])])" );
      (* Higher-order function: calling a function expression *)
      ( "higher-order function call",
        "(fun(x: int) -> int = x + 1)(42)",
        "ECall(EFun([x:int], int, EBinOp(Add, EVar(x), EInt(1))), [EInt(42)])"
      );
      (* Function call on let-bound function *)
      ( "let-bound function call",
        "let f = fun(x: int) -> int = x * 2 in f(5)",
        "ELet(f, EFun([x:int], int, EBinOp(Mul, EVar(x), EInt(2))), \
         ECall(EVar(f), [EInt(5)]))" );
      (* Higher-order function passing function as argument *)
      ( "passing function as argument",
        "apply(double, 5)",
        "ECall(EVar(apply), [EVar(double); EInt(5)])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test function call errors *)
let test_function_call_errors () =
  let test_cases =
    [
      (* Missing closing parenthesis *)
      ("function call unclosed", "foo(42");
      (* Missing opening parenthesis *)
      ("function call no open", "foo 42)");
      (* Invalid argument expressions *)
      ("function call invalid arg", "foo(1 +)");
      (* Trailing comma *)
      ("function call trailing comma", "foo(1, 2,)");
      (* Double comma *)
      ("function call double comma", "foo(1,, 2)");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test array access expressions *)
let test_array_access () =
  let test_cases =
    [
      (* Simple array access *)
      ("simple array access", "arr[0]", "EArrayAccess(EVar(arr), EInt(0))");
      ( "array access with variable",
        "arr[i]",
        "EArrayAccess(EVar(arr), EVar(i))" );
      (* Array access with expressions *)
      ( "array access with expression",
        "arr[i + 1]",
        "EArrayAccess(EVar(arr), EBinOp(Add, EVar(i), EInt(1)))" );
      ( "array access with complex expression",
        "data[x * 2 + 1]",
        "EArrayAccess(EVar(data), EBinOp(Add, EBinOp(Mul, EVar(x), EInt(2)), \
         EInt(1)))" );
      (* Nested array access *)
      ( "nested array access",
        "matrix[i][j]",
        "EArrayAccess(EArrayAccess(EVar(matrix), EVar(i)), EVar(j))" );
      ( "triple nested array access",
        "cube[x][y][z]",
        "EArrayAccess(EArrayAccess(EArrayAccess(EVar(cube), EVar(x)), \
         EVar(y)), EVar(z))" );
      (* Array access with function calls *)
      ( "array access with function call index",
        "arr[getIndex()]",
        "EArrayAccess(EVar(arr), ECall(EVar(getIndex), []))" );
      (* Array access in expressions *)
      ( "array access in arithmetic",
        "arr[0] + arr[1]",
        "EBinOp(Add, EArrayAccess(EVar(arr), EInt(0)), EArrayAccess(EVar(arr), \
         EInt(1)))" );
      (* Array access with different index types *)
      ( "array access with char index",
        "charMap['a']",
        "EArrayAccess(EVar(charMap), EChar('a'))" );
      ( "array access with boolean expression",
        "flags[x > 0]",
        "EArrayAccess(EVar(flags), EBinOp(Gt, EVar(x), EInt(0)))" );
      (* Array access with tuple as index *)
      ( "array access with tuple index",
        "map[(x, y)]",
        "EArrayAccess(EVar(map), ETuple([EVar(x); EVar(y)]))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test array access errors *)
let test_array_access_errors () =
  let test_cases =
    [
      (* Missing closing bracket *)
      ("array access unclosed", "arr[0");
      (* Missing opening bracket *)
      ("array access no open", "arr 0]");
      (* Missing index *)
      ("array access no index", "arr[]");
      (* Invalid index expressions *)
      ("array access invalid index", "arr[1 +]");
      (* Nested bracket issues *)
      ("array access nested unclosed", "arr[matrix[i]");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test byte literal expressions *)
let test_byte_literals () =
  let test_cases =
    [
      (* Single byte *)
      ("single byte", "[0x48]", "EBytes([0x48])");
      (* Multiple bytes *)
      ( "multiple bytes",
        "[0x48, 0x65, 0x6C, 0x6C, 0x6F]",
        "EBytes([0x48; 0x65; 0x6C; 0x6C; 0x6F])" );
      (* Mixed case hex *)
      ("mixed case hex", "[0xAB, 0xcd, 0xEF]", "EBytes([0xAB; 0xCD; 0xEF])");
      (* All values 0x00 to 0xFF *)
      ("min max bytes", "[0x00, 0xFF]", "EBytes([0x00; 0xFF])");
      (* Spaces around commas *)
      ( "spaced bytes",
        "[0x20,0x21, 0x22 , 0x23]",
        "EBytes([0x20; 0x21; 0x22; 0x23])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test byte literal errors *)
let test_byte_literal_errors () =
  let test_cases =
    [
      (* Unterminated byte array *)
      ("unterminated bytes", "[0x48, 0x65");
      (* Invalid hex digit *)
      ("invalid hex digit", "[0xGH]");
      (* Out of range value *)
      ("out of range", "[0x100]");
      (* Missing comma *)
      ("missing comma", "[0x48 0x65]");
      (* Single hex digit - should have two digits after 0x *)
      ("single hex digit", "[0xA]");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Helper function to parse program string input *)
let parse_program_string input =
  try
    let tokens = lex input in
    parse_program_with_error_handling tokens
  with Lexer.Lex_error msg -> Error ("Lexer error: " ^ msg)

(** Convert program AST to string for testing *)
let rec program_to_string program =
  let imports_str =
    String.concat "; " (List.map import_to_string program.imports)
  in
  let type_defs_str =
    String.concat "; " (List.map type_def_to_string program.type_defs)
  in
  let functions_str =
    String.concat "; " (List.map func_to_string program.functions)
  in
  Printf.sprintf "Program{package=%s; imports=[%s]; types=[%s]; functions=[%s]}"
    program.package_name imports_str type_defs_str functions_str

and import_to_string import =
  match import.alias with
  | None -> Printf.sprintf "Import(%s)" import.package_path
  | Some alias -> Printf.sprintf "Import(%s as %s)" import.package_path alias

and type_def_to_string = function
  | TAlias (name, typ) ->
      Printf.sprintf "TAlias(%s, %s)" name (type_to_string typ)
  | TStruct (name, fields) ->
      let fields_str =
        String.concat "; " (List.map struct_field_to_string fields)
      in
      Printf.sprintf "TStruct(%s, [%s])" name fields_str
  | TUnion (name, variants) ->
      let variants_str =
        String.concat "; " (List.map union_variant_to_string variants)
      in
      Printf.sprintf "TUnion(%s, [%s])" name variants_str

and struct_field_to_string field =
  Printf.sprintf "%s:%s" field.field_name (type_to_string field.field_type)

and union_variant_to_string variant =
  match variant.variant_data with
  | None -> variant.variant_name
  | Some typ ->
      Printf.sprintf "%s(%s)" variant.variant_name (type_to_string typ)

and func_to_string func =
  let params_str =
    String.concat "; "
      (List.map
         (fun (name, typ) -> Printf.sprintf "%s:%s" name (type_to_string typ))
         func.params)
  in
  Printf.sprintf "Func(%s, [%s], %s)" func.fname params_str
    (type_to_string func.ret)

(** Test package system parsing *)
let test_package_system () =
  let test_cases =
    [
      (* Simple package declaration *)
      ( "simple package",
        "package math",
        "Program{package=math; imports=[]; types=[]; functions=[]}" );
      (* Package with single import *)
      ( "package with import",
        "package main\nimport \"math\"",
        "Program{package=main; imports=[Import(math)]; types=[]; functions=[]}"
      );
      (* Package with multiple imports *)
      ( "package with multiple imports",
        "package main\nimport \"math\"\nimport \"strings\"",
        "Program{package=main; imports=[Import(math); Import(strings)]; \
         types=[]; functions=[]}" );
      (* Package with aliased import *)
      ( "package with aliased import",
        "package main\nimport m \"math\"",
        "Program{package=main; imports=[Import(math as m)]; types=[]; \
         functions=[]}" );
      (* Package with type alias *)
      ( "package with type alias",
        "package main\ntype UserName = string",
        "Program{package=main; imports=[]; types=[TAlias(UserName, string)]; \
         functions=[]}" );
      (* Package with struct definition *)
      ( "package with struct",
        "package main\ntype Point = struct { x: int; y: int }",
        "Program{package=main; imports=[]; types=[TStruct(Point, [x:int; \
         y:int])]; functions=[]}" );
      (* Package with union definition *)
      ( "package with union",
        "package main\ntype Option = union { Some(int) | None }",
        "Program{package=main; imports=[]; types=[TUnion(Option, [Some(int); \
         None])]; functions=[]}" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_program_string input with
      | Ok program ->
          let actual = program_to_string program in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test package system errors *)
let test_package_system_errors () =
  let test_cases =
    [
      (* Missing package name *)
      ("missing package name", "package");
      (* Invalid package name *)
      ("invalid package name", "package 123");
      (* Missing import path *)
      ("missing import path", "package main\nimport");
      (* Invalid import path *)
      ("invalid import path", "package main\nimport 123");
      (* Missing type name *)
      ("missing type name", "package main\ntype");
      (* Invalid type definition *)
      ("invalid type definition", "package main\ntype UserName");
      (* Incomplete struct *)
      ("incomplete struct", "package main\ntype Point = struct {");
      (* Incomplete union *)
      ("incomplete union", "package main\ntype Option = union {");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_program_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test block expressions *)
let test_block_expressions () =
  let test_cases =
    [
      (* Empty block *)
      ("empty block", "{}", "EUnit");
      (* Single expression block *)
      ("single expr block", "{ 42 }", "EBlock([EInt(42)])");
      (* Multiple expressions block *)
      ( "multiple expr block",
        "{ 1; 2; 3 }",
        "EBlock([EInt(1); EInt(2); EInt(3)])" );
      (* Block with let bindings *)
      ( "block with let",
        "{ let x = 1 in x; x + 2 }",
        "EBlock([ELet(x, EInt(1), EVar(x)); EBinOp(Add, EVar(x), EInt(2))])" );
      (* Block with mixed expressions *)
      ( "block with mixed exprs",
        "{ true; \"hello\"; 42 }",
        "EBlock([EBool(true); EString(\"hello\"); EInt(42)])" );
      (* Nested blocks *)
      ( "nested blocks",
        "{ { 1; 2 }; 3 }",
        "EBlock([EBlock([EInt(1); EInt(2)]); EInt(3)])" );
      (* Block with function call *)
      ( "block with function call",
        "{ f(); 42 }",
        "EBlock([ECall(EVar(f), []); EInt(42)])" );
      (* Block with if expression *)
      ( "block with if",
        "{ if true then 1 else 2; 3 }",
        "EBlock([EIf(EBool(true), EInt(1), EInt(2)); EInt(3)])" );
      (* Block with tuples *)
      ( "block with tuples",
        "{ (1, 2); (3, 4) }",
        "EBlock([ETuple([EInt(1); EInt(2)]); ETuple([EInt(3); EInt(4)])])" );
      (* Block with option constructors *)
      ( "block with option",
        "{ Some(1); None }",
        "EBlock([ESome(EInt(1)); ENone])" );
      (* Complex nested block with let and arithmetic *)
      ( "complex nested block",
        "{ let x = { 1; 2 } in x; x + 3 }",
        "EBlock([ELet(x, EBlock([EInt(1); EInt(2)]), EVar(x)); EBinOp(Add, \
         EVar(x), EInt(3))])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test block expression errors *)
let test_block_expression_errors () =
  let test_cases =
    [
      (* Unclosed block *)
      ("unclosed block", "{ 1; 2");
      (* Missing semicolon *)
      ("missing semicolon", "{ 1 2 }");
      (* Invalid expression in block *)
      ("invalid expr in block", "{ 1 + }");
      (* Double semicolon *)
      ("double semicolon", "{ 1;; 2 }");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test array literal expressions *)
let test_array_literals () =
  let test_cases =
    [
      (* Empty array *)
      ("empty array", "[]", "EArray([])");
      (* Single element array *)
      ("single element array", "[42]", "EArray([EInt(42)])");
      (* Multiple element array *)
      ( "multiple element array",
        "[1, 2, 3]",
        "EArray([EInt(1); EInt(2); EInt(3)])" );
      (* Array with different types (but all should be the same type) *)
      ( "array with expressions",
        "[1 + 2, 3 * 4, 5]",
        "EArray([EBinOp(Add, EInt(1), EInt(2)); EBinOp(Mul, EInt(3), EInt(4)); \
         EInt(5)])" );
      (* Array with variables *)
      ( "array with variables",
        "[x, y, z]",
        "EArray([EVar(x); EVar(y); EVar(z)])" );
      (* Array with strings *)
      ( "string array",
        "[\"hello\", \"world\"]",
        "EArray([EString(\"hello\"); EString(\"world\")])" );
      (* Array with booleans *)
      ( "boolean array",
        "[true, false, true]",
        "EArray([EBool(true); EBool(false); EBool(true)])" );
      (* Nested arrays *)
      ( "nested arrays",
        "[[1, 2], [3, 4]]",
        "EArray([EArray([EInt(1); EInt(2)]); EArray([EInt(3); EInt(4)])])" );
      (* Array with tuples *)
      ( "array of tuples",
        "[(1, 2), (3, 4)]",
        "EArray([ETuple([EInt(1); EInt(2)]); ETuple([EInt(3); EInt(4)])])" );
      (* Array with function calls *)
      ( "array with function calls",
        "[f(1), g(2), h(3)]",
        "EArray([ECall(EVar(f), [EInt(1)]); ECall(EVar(g), [EInt(2)]); \
         ECall(EVar(h), [EInt(3)])])" );
      (* Array with Option values *)
      ( "array with options",
        "[Some(1), None, Some(2)]",
        "EArray([ESome(EInt(1)); ENone; ESome(EInt(2))])" );
      (* Complex nested expression *)
      ( "complex array expression",
        "[if true then 1 else 2, let x = 3 in x]",
        "EArray([EIf(EBool(true), EInt(1), EInt(2)); ELet(x, EInt(3), \
         EVar(x))])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test array literal errors *)
let test_array_literal_errors () =
  let test_cases =
    [
      (* Unclosed array *)
      ("unclosed array", "[1, 2, 3");
      (* Missing comma *)
      ("missing comma", "[1 2 3]");
      (* Trailing comma *)
      ("trailing comma", "[1, 2,]");
      (* Invalid expression in array *)
      ("invalid expr in array", "[1 + , 2]");
      (* Double comma *)
      ("double comma", "[1,, 2]");
      (* Missing closing bracket in nested array *)
      ("nested unclosed", "[[1, 2]");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test unary operators *)
let test_unary_operators () =
  let test_cases =
    [
      (* Unary negation *)
      ("negative integer", "-42", "EUnOp(Neg, EInt(42))");
      ("negative float", "-3.14", "EUnOp(Neg, EFloat(3.14))");
      ("negative variable", "-x", "EUnOp(Neg, EVar(x))");
      ("double negation", "--42", "EUnOp(Neg, EUnOp(Neg, EInt(42)))");
      (* Unary negation with expressions *)
      ("negative expr", "-(1 + 2)", "EUnOp(Neg, EBinOp(Add, EInt(1), EInt(2)))");
      ("negative function call", "-(f())", "EUnOp(Neg, ECall(EVar(f), []))");
      (* Shows that -f() is parsed as (-f)() *)
      ("negative func then call", "-f()", "ECall(EUnOp(Neg, EVar(f)), [])");
      (* Logical not already tested but add more cases *)
      ("not variable", "!x", "EUnOp(Not, EVar(x))");
      ("double not", "!!true", "EUnOp(Not, EUnOp(Not, EBool(true)))");
      ( "not expression",
        "!(x && y)",
        "EUnOp(Not, EBinOp(And, EVar(x), EVar(y)))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test tuple errors *)
let test_tuple_errors () =
  let test_cases =
    [
      (* Unclosed tuple *)
      ("unclosed tuple", "(1, 2");
      (* Trailing comma *)
      ("tuple trailing comma", "(1, 2,)");
      (* Missing comma *)
      ("tuple missing comma", "(1 2)");
      (* Empty tuple with comma *)
      ("empty tuple with comma", "(,)");
      (* Double comma *)
      ("tuple double comma", "(1,, 2)");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test chained postfix expressions *)
let test_chained_postfix () =
  let test_cases =
    [
      (* Chained function calls *)
      ("chained calls", "f()()", "ECall(ECall(EVar(f), []), [])");
      ("triple calls", "f()()()", "ECall(ECall(ECall(EVar(f), []), []), [])");
      (* Chained array access *)
      ( "chained array",
        "arr[0][1]",
        "EArrayAccess(EArrayAccess(EVar(arr), EInt(0)), EInt(1))" );
      (* Mixed chaining *)
      ("call then array", "f()[0]", "EArrayAccess(ECall(EVar(f), []), EInt(0))");
      ( "array then call",
        "arr[0]()",
        "ECall(EArrayAccess(EVar(arr), EInt(0)), [])" );
      (* Complex chaining *)
      ( "complex chain",
        "getArray()[0](x)[1]",
        "EArrayAccess(ECall(EArrayAccess(ECall(EVar(getArray), []), EInt(0)), \
         [EVar(x)]), EInt(1))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test empty parameter functions *)
let test_empty_param_functions () =
  let test_cases =
    [
      (* No-parameter function *)
      ("empty params", "fun() -> int = 42", "EFun([], int, EInt(42))");
      ("empty params unit", "fun() -> unit = ()", "EFun([], unit, EUnit)");
      (* Let-bound empty param function *)
      ( "let empty params",
        "let f = fun() -> int = 42 in f()",
        "ELet(f, EFun([], int, EInt(42)), ECall(EVar(f), []))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test complex pattern matching errors *)
let test_pattern_errors () =
  let test_cases =
    [
      (* Invalid patterns *)
      ("invalid pattern", "match x with | 1 + 2 -> 0");
      (* Incomplete patterns *)
      ("incomplete Some", "match x with | Some( -> 1");
      ("incomplete Ok", "match x with | Ok( -> 1");
      (* Invalid tuple patterns *)
      ("tuple pattern trailing comma", "match x with | (1, 2,) -> 0");
      ("invalid tuple pattern", "match x with | (1 2) -> 0");
      (* Missing components *)
      ("missing arrow", "match x with | 1 0");
      ("missing body", "match x with | 1 ->");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test extremely complex nested expressions *)
let test_extreme_complexity () =
  let test_cases =
    [
      (* Deeply nested let expressions with pattern matching *)
      ( "deep let with match",
        "let x = let y = match Some(42) with | Some(n) -> n * 2 | None -> 0 in \
         let z = if y > 10 then Ok(y) else Err(\"too small\") in match z with \
         | Ok(v) -> Some(v + 1) | Err(_) -> None in match x with | Some(v) -> \
         v | None -> -1",
        "ELet(x, ELet(y, EMatch(ESome(EInt(42)), [{pattern=PSome(PVar(n)); \
         body=EBinOp(Mul, EVar(n), EInt(2))}; {pattern=PNone; body=EInt(0)}]), \
         ELet(z, EIf(EBinOp(Gt, EVar(y), EInt(10)), EOk(EVar(y)), \
         EErr(EString(\"too small\"))), EMatch(EVar(z), \
         [{pattern=POk(PVar(v)); body=ESome(EBinOp(Add, EVar(v), EInt(1)))}; \
         {pattern=PErr(PWildcard); body=ENone}]))), EMatch(EVar(x), \
         [{pattern=PSome(PVar(v)); body=EVar(v)}; {pattern=PNone; \
         body=EUnOp(Neg, EInt(1))}]))" );
      (* Complex nested match and let *)
      ( "complex nested match let",
        "let process = fun(opt: int) -> int = match Some(opt) with | Some(x) \
         -> let y = x * 2 in if y > 10 then y else 10 | None -> 0 in \
         process(21)",
        "ELet(process, EFun([opt:int], int, EMatch(ESome(EVar(opt)), \
         [{pattern=PSome(PVar(x)); body=ELet(y, EBinOp(Mul, EVar(x), EInt(2)), \
         EIf(EBinOp(Gt, EVar(y), EInt(10)), EVar(y), EInt(10)))}; \
         {pattern=PNone; body=EInt(0)}])), ECall(EVar(process), [EInt(21)]))" );
      (* Nested arrays with complex expressions *)
      ( "nested array comprehension simulation",
        "[[if i > j then i * j else 0, j + 1], [i - 1, if i == j then 1 else \
         0]]",
        "EArray([EArray([EIf(EBinOp(Gt, EVar(i), EVar(j)), EBinOp(Mul, \
         EVar(i), EVar(j)), EInt(0)); EBinOp(Add, EVar(j), EInt(1))]); \
         EArray([EBinOp(Sub, EVar(i), EInt(1)); EIf(EBinOp(Eq, EVar(i), \
         EVar(j)), EInt(1), EInt(0))])])" );
      (* Complex block with side effects and control flow *)
      ( "complex block flow",
        "{ let x = processData() in let result = match x with | Ok(data) -> { \
         let t = transform(data) in t; Some(t) } | Err(e) -> { log(e); None } \
         in if result != None then result else Some(defaultValue()) }",
        "EBlock([ELet(x, ECall(EVar(processData), []), ELet(result, \
         EMatch(EVar(x), [{pattern=POk(PVar(data)); body=EBlock([ELet(t, \
         ECall(EVar(transform), [EVar(data)]), EVar(t)); ESome(EVar(t))])}; \
         {pattern=PErr(PVar(e)); body=EBlock([ECall(EVar(log), [EVar(e)]); \
         ENone])}]), EIf(EBinOp(Neq, EVar(result), ENone), EVar(result), \
         ESome(ECall(EVar(defaultValue), [])))))])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test operator precedence edge cases *)
let test_precedence_edge_cases () =
  let test_cases =
    [
      (* Unary operators with binary operators *)
      ( "unary vs binary precedence",
        "-x * y + !z && w",
        "EBinOp(And, EBinOp(Add, EBinOp(Mul, EUnOp(Neg, EVar(x)), EVar(y)), \
         EUnOp(Not, EVar(z))), EVar(w))" );
      (* Complex arithmetic with function calls *)
      ( "arithmetic with calls",
        "f(x) + g(y) * h(z) - i(w) / j(v)",
        "EBinOp(Sub, EBinOp(Add, ECall(EVar(f), [EVar(x)]), EBinOp(Mul, \
         ECall(EVar(g), [EVar(y)]), ECall(EVar(h), [EVar(z)]))), EBinOp(Div, \
         ECall(EVar(i), [EVar(w)]), ECall(EVar(j), [EVar(v)])))" );
      (* Nested conditional expressions *)
      ( "nested conditionals precedence",
        "if a then if b then c else d else if e then f else g",
        "EIf(EVar(a), EIf(EVar(b), EVar(c), EVar(d)), EIf(EVar(e), EVar(f), \
         EVar(g)))" );
      (* Mixed logical operators with comparisons *)
      ( "complex logical expression",
        "a > b && c < d || e == f && g != h || i >= j && k <= l",
        "EBinOp(Or, EBinOp(Or, EBinOp(And, EBinOp(Gt, EVar(a), EVar(b)), \
         EBinOp(Lt, EVar(c), EVar(d))), EBinOp(And, EBinOp(Eq, EVar(e), \
         EVar(f)), EBinOp(Neq, EVar(g), EVar(h)))), EBinOp(And, EBinOp(Ge, \
         EVar(i), EVar(j)), EBinOp(Le, EVar(k), EVar(l))))" );
      (* Array access with arithmetic *)
      ( "array arithmetic precedence",
        "arr[i + 1] * arr[j - 1] + arr[k * 2]",
        "EBinOp(Add, EBinOp(Mul, EArrayAccess(EVar(arr), EBinOp(Add, EVar(i), \
         EInt(1))), EArrayAccess(EVar(arr), EBinOp(Sub, EVar(j), EInt(1)))), \
         EArrayAccess(EVar(arr), EBinOp(Mul, EVar(k), EInt(2))))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test error recovery and specific error messages *)
let test_error_recovery () =
  let test_cases =
    [
      (* Multiple errors in one expression *)
      ("multiple errors", "let x = 1 + in let y = 2 * in x + y");
      (* Deeply nested error *)
      ("nested error", "match Some(Ok(Err())) with | Some(Ok(Err( -> 1");
      (* Error in function body *)
      ("function body error", "fun(x: int) -> int = x +");
      (* Incomplete block *)
      ("incomplete block", "{ let x = 1; let y = 2; x +");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test struct literal expressions *)
let test_struct_literals () =
  let test_cases =
    [
      (* Empty struct *)
      ("empty struct", "{}", "EUnit");
      (* Single field struct *)
      ("single field struct", "{ x = 42 }", "EStruct([x = EInt(42)])");
      (* Multi-field struct *)
      ( "multi-field struct",
        "{ x = 1, y = 2 }",
        "EStruct([x = EInt(1); y = EInt(2)])" );
      (* Struct with different types *)
      ( "mixed type struct",
        "{ name = \"John\", age = 30, active = true }",
        "EStruct([name = EString(\"John\"); age = EInt(30); active = \
         EBool(true)])" );
      (* Struct with expressions *)
      ( "struct with expressions",
        "{ x = 1 + 2, y = f(3) }",
        "EStruct([x = EBinOp(Add, EInt(1), EInt(2)); y = ECall(EVar(f), \
         [EInt(3)])])" );
      (* Struct with variables *)
      ( "struct with variables",
        "{ a = x, b = y }",
        "EStruct([a = EVar(x); b = EVar(y)])" );
      (* Nested struct *)
      ( "nested struct",
        "{ point = { x = 1, y = 2 }, color = \"red\" }",
        "EStruct([point = EStruct([x = EInt(1); y = EInt(2)]); color = \
         EString(\"red\")])" );
      (* Struct with tuple *)
      ( "struct with tuple",
        "{ position = (1, 2), size = (10, 20) }",
        "EStruct([position = ETuple([EInt(1); EInt(2)]); size = \
         ETuple([EInt(10); EInt(20)])])" );
      (* Struct with array *)
      ( "struct with array",
        "{ values = [1, 2, 3], count = 3 }",
        "EStruct([values = EArray([EInt(1); EInt(2); EInt(3)]); count = \
         EInt(3)])" );
      (* Struct with complex expressions *)
      ( "complex struct",
        "{ result = if x > 0 then Some(x) else None, processed = true }",
        "EStruct([result = EIf(EBinOp(Gt, EVar(x), EInt(0)), ESome(EVar(x)), \
         ENone); processed = EBool(true)])" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test struct literal errors *)
let test_struct_literal_errors () =
  let test_cases =
    [
      (* Missing field value *)
      ("missing field value", "{ x = }");
      (* Missing equals sign *)
      ("missing equals", "{ x 42 }");
      (* Unclosed struct *)
      ("unclosed struct", "{ x = 42, y = 24");
      (* Trailing comma *)
      ("trailing comma", "{ x = 42, y = 24, }");
      (* Missing field name *)
      ("missing field name", "{ = 42 }");
      (* Invalid field name *)
      ("invalid field name", "{ 123 = 42 }");
      (* Missing comma *)
      ("missing comma", "{ x = 42 y = 24 }");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test field access expressions *)
let test_field_access () =
  let test_cases =
    [
      (* Simple field access *)
      ("simple field access", "obj.field", "EFieldAccess(EVar(obj), field)");
      (* Chained field access *)
      ( "chained field access",
        "obj.field1.field2",
        "EFieldAccess(EFieldAccess(EVar(obj), field1), field2)" );
      (* Field access with function call *)
      ( "field access with call",
        "obj.method()",
        "ECall(EFieldAccess(EVar(obj), method), [])" );
      (* Field access with array access *)
      ( "field access with array",
        "obj.array[0]",
        "EArrayAccess(EFieldAccess(EVar(obj), array), EInt(0))" );
      (* Field access on function call result *)
      ( "field access on call result",
        "getObj().field",
        "EFieldAccess(ECall(EVar(getObj), []), field)" );
      (* Field access on array element *)
      ( "field access on array element",
        "objects[0].name",
        "EFieldAccess(EArrayAccess(EVar(objects), EInt(0)), name)" );
      (* Complex chained operations *)
      ( "complex chained operations",
        "data.items[0].process().result",
        "EFieldAccess(ECall(EFieldAccess(EArrayAccess(EFieldAccess(EVar(data), \
         items), EInt(0)), process), []), result)" );
      (* Field access with struct literal *)
      ( "field access with struct",
        "{ x = 1, y = 2 }.x",
        "EFieldAccess(EStruct([x = EInt(1); y = EInt(2)]), x)" );
      (* Field access in expression *)
      ( "field access in expression",
        "point.x + point.y",
        "EBinOp(Add, EFieldAccess(EVar(point), x), EFieldAccess(EVar(point), \
         y))" );
      (* Field access with parentheses *)
      ( "field access with parentheses",
        "(obj.field).other",
        "EFieldAccess(EFieldAccess(EVar(obj), field), other)" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test field access errors *)
let test_field_access_errors () =
  let test_cases =
    [
      (* Missing field name *)
      ("missing field name", "obj.");
      (* Invalid field name *)
      ("invalid field name", "obj.123");
      (* Double dot *)
      ("double dot", "obj..field");
      (* Field access on invalid expression *)
      ("field access on number", "42.field");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test assignment operator expressions *)
let test_assignment_operators () =
  let test_cases =
    [
      (* Simple assignment *)
      ("simple assignment", "x := 42", "EBinOp(Assign, EVar(x), EInt(42))");
      (* Assignment with expression *)
      ( "assignment with expression",
        "x := y + 1",
        "EBinOp(Assign, EVar(x), EBinOp(Add, EVar(y), EInt(1)))" );
      (* Right-associative assignment *)
      ( "right associative assignment",
        "x := y := 42",
        "EBinOp(Assign, EVar(x), EBinOp(Assign, EVar(y), EInt(42)))" );
      (* Assignment with field access *)
      ( "assignment with field access",
        "obj.field := value",
        "EBinOp(Assign, EFieldAccess(EVar(obj), field), EVar(value))" );
      (* Assignment with array access *)
      ( "assignment with array access",
        "arr[0] := 42",
        "EBinOp(Assign, EArrayAccess(EVar(arr), EInt(0)), EInt(42))" );
      (* Assignment with function call result *)
      ( "assignment with function call",
        "x := f(1, 2)",
        "EBinOp(Assign, EVar(x), ECall(EVar(f), [EInt(1); EInt(2)]))" );
      (* Assignment with complex expression *)
      ( "assignment with complex expression",
        "x := if a > b then a else b",
        "EBinOp(Assign, EVar(x), EIf(EBinOp(Gt, EVar(a), EVar(b)), EVar(a), \
         EVar(b)))" );
      (* Assignment with tuple *)
      ( "assignment with tuple",
        "x := (1, 2)",
        "EBinOp(Assign, EVar(x), ETuple([EInt(1); EInt(2)]))" );
      (* Assignment with struct literal *)
      ( "assignment with struct",
        "x := { a = 1, b = 2 }",
        "EBinOp(Assign, EVar(x), EStruct([a = EInt(1); b = EInt(2)]))" );
      (* Assignment precedence with arithmetic *)
      ( "assignment precedence",
        "x := y + z * 2",
        "EBinOp(Assign, EVar(x), EBinOp(Add, EVar(y), EBinOp(Mul, EVar(z), \
         EInt(2))))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test pipe operator expressions *)
let test_pipe_operators () =
  let test_cases =
    [
      (* Simple pipe *)
      ("simple pipe", "x |> f", "EBinOp(Pipe, EVar(x), EVar(f))");
      (* Pipe with function call *)
      ( "pipe with function call",
        "x |> f(1)",
        "EBinOp(Pipe, EVar(x), ECall(EVar(f), [EInt(1)]))" );
      (* Left-associative pipe chain *)
      ( "left associative pipe",
        "x |> f |> g",
        "EBinOp(Pipe, EBinOp(Pipe, EVar(x), EVar(f)), EVar(g))" );
      (* Pipe with arithmetic *)
      ( "pipe with arithmetic",
        "x + 1 |> f",
        "EBinOp(Pipe, EBinOp(Add, EVar(x), EInt(1)), EVar(f))" );
      (* Complex pipe chain *)
      ( "complex pipe chain",
        "data |> filter(pred) |> map(transform) |> collect",
        "EBinOp(Pipe, EBinOp(Pipe, EBinOp(Pipe, EVar(data), \
         ECall(EVar(filter), [EVar(pred)])), ECall(EVar(map), \
         [EVar(transform)])), EVar(collect))" );
      (* Pipe with field access *)
      ( "pipe with field access",
        "obj.value |> process",
        "EBinOp(Pipe, EFieldAccess(EVar(obj), value), EVar(process))" );
      (* Pipe with array access *)
      ( "pipe with array access",
        "arr[0] |> transform",
        "EBinOp(Pipe, EArrayAccess(EVar(arr), EInt(0)), EVar(transform))" );
      (* Pipe with struct literal *)
      ( "pipe with struct",
        "{ x = 1, y = 2 } |> process",
        "EBinOp(Pipe, EStruct([x = EInt(1); y = EInt(2)]), EVar(process))" );
      (* Pipe with conditional *)
      ( "pipe with conditional",
        "if a then b else c |> f",
        "EIf(EVar(a), EVar(b), EBinOp(Pipe, EVar(c), EVar(f)))" );
      (* Pipe precedence test *)
      ( "pipe precedence",
        "(x |> f) && (y |> g)",
        "EBinOp(And, EBinOp(Pipe, EVar(x), EVar(f)), EBinOp(Pipe, EVar(y), \
         EVar(g)))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test assignment and pipe operator errors *)
let test_assignment_pipe_errors () =
  let test_cases =
    [
      (* Missing right operand for assignment *)
      ("assignment missing right operand", "x :=");
      (* Missing left operand for assignment *)
      ("assignment missing left operand", ":= 42");
      (* Missing right operand for pipe *)
      ("pipe missing right operand", "x |>");
      (* Missing left operand for pipe *)
      ("pipe missing left operand", "|> f");
      (* Missing assignment operator syntax *)
      ("incomplete assignment", "x := := y");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

(** Test while loop expressions *)
let test_while_loops () =
  let test_cases =
    [
      (* Simple while loop *)
      ("simple while loop", "while true do x", "EWhile(EBool(true), EVar(x))");
      (* While loop with condition *)
      ( "while with condition",
        "while x > 0 do x := x - 1",
        "EWhile(EBinOp(Gt, EVar(x), EInt(0)), EBinOp(Assign, EVar(x), \
         EBinOp(Sub, EVar(x), EInt(1))))" );
      (* While loop with block body *)
      ( "while with block",
        "while x < 10 do { x := x + 1; print(x) }",
        "EWhile(EBinOp(Lt, EVar(x), EInt(10)), EBlock([EBinOp(Assign, EVar(x), \
         EBinOp(Add, EVar(x), EInt(1))); ECall(EVar(print), [EVar(x)])]))" );
      (* Nested while loops *)
      ( "nested while",
        "while x > 0 do while y > 0 do y := y - 1",
        "EWhile(EBinOp(Gt, EVar(x), EInt(0)), EWhile(EBinOp(Gt, EVar(y), \
         EInt(0)), EBinOp(Assign, EVar(y), EBinOp(Sub, EVar(y), EInt(1)))))" );
      (* While with complex condition *)
      ( "while complex condition",
        "while x > 0 && y < 10 do process(x, y)",
        "EWhile(EBinOp(And, EBinOp(Gt, EVar(x), EInt(0)), EBinOp(Lt, EVar(y), \
         EInt(10))), ECall(EVar(process), [EVar(x); EVar(y)]))" );
      (* While with function call condition *)
      ( "while with function call",
        "while hasNext() do process(next())",
        "EWhile(ECall(EVar(hasNext), []), ECall(EVar(process), \
         [ECall(EVar(next), [])]))" );
      (* While with struct field access *)
      ( "while with field access",
        "while obj.running do obj.step()",
        "EWhile(EFieldAccess(EVar(obj), running), \
         ECall(EFieldAccess(EVar(obj), step), []))" );
      (* While with array access *)
      ( "while with array access",
        "while arr[i] != 0 do i := i + 1",
        "EWhile(EBinOp(Neq, EArrayAccess(EVar(arr), EVar(i)), EInt(0)), \
         EBinOp(Assign, EVar(i), EBinOp(Add, EVar(i), EInt(1))))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test for loop expressions *)
let test_for_loops () =
  let test_cases =
    [
      (* Simple for loop *)
      ( "simple for loop",
        "for x in arr do print(x)",
        "EFor(x, EVar(arr), ECall(EVar(print), [EVar(x)]))" );
      (* For loop with expression body *)
      ( "for with expression",
        "for item in items do process(item)",
        "EFor(item, EVar(items), ECall(EVar(process), [EVar(item)]))" );
      (* For loop with block body *)
      ( "for with block",
        "for i in range do { print(i); total := total + i }",
        "EFor(i, EVar(range), EBlock([ECall(EVar(print), [EVar(i)]); \
         EBinOp(Assign, EVar(total), EBinOp(Add, EVar(total), EVar(i)))]))" );
      (* For loop over array literal *)
      ( "for over array literal",
        "for x in [1, 2, 3] do sum := sum + x",
        "EFor(x, EArray([EInt(1); EInt(2); EInt(3)]), EBinOp(Assign, \
         EVar(sum), EBinOp(Add, EVar(sum), EVar(x))))" );
      (* For loop over function call result *)
      ( "for over function call",
        "for item in getItems() do process(item)",
        "EFor(item, ECall(EVar(getItems), []), ECall(EVar(process), \
         [EVar(item)]))" );
      (* For loop over field access *)
      ( "for over field access",
        "for element in obj.elements do handle(element)",
        "EFor(element, EFieldAccess(EVar(obj), elements), ECall(EVar(handle), \
         [EVar(element)]))" );
      (* Nested for loops *)
      ( "nested for loops",
        "for row in matrix do for cell in row do process(cell)",
        "EFor(row, EVar(matrix), EFor(cell, EVar(row), ECall(EVar(process), \
         [EVar(cell)])))" );
      (* For loop with complex iterable *)
      ( "for with complex iterable",
        "for x in filter(predicate, data) do transform(x)",
        "EFor(x, ECall(EVar(filter), [EVar(predicate); EVar(data)]), \
         ECall(EVar(transform), [EVar(x)]))" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      match parse_string input with
      | Ok expr ->
          let actual = expr_to_string expr in
          Test.check Test.string name expected actual
      | Error msg -> failwith (Printf.sprintf "Parse error in %s: %s" name msg))
    test_cases

(** Test loop expression errors *)
let test_loop_errors () =
  let test_cases =
    [
      (* Missing condition in while *)
      ("while missing condition", "while do x");
      (* Missing do in while *)
      ("while missing do", "while true x");
      (* Missing body in while *)
      ("while missing body", "while true do");
      (* Missing variable in for *)
      ("for missing variable", "for in arr do x");
      (* Missing in keyword in for *)
      ("for missing in", "for x arr do x");
      (* Missing iterable in for *)
      ("for missing iterable", "for x in do body");
      (* Missing do in for *)
      ("for missing do", "for x in arr body");
      (* Missing body in for *)
      ("for missing body", "for x in arr do");
      (* Invalid variable name in for *)
      ("for invalid variable", "for 123 in arr do x");
    ]
  in
  List.iter
    (fun (name, input) ->
      match parse_string input with
      | Ok _ ->
          failwith
            (Printf.sprintf "Expected parse error for %s but got success" name)
      | Error _ -> ()) (* Expected error *)
    test_cases

let () =
  Test.run "SNOW Parser Tests"
    [
      ("literals", [ Test.test_case "basic literals" `Quick test_literals ]);
      ("variables", [ Test.test_case "variable parsing" `Quick test_variables ]);
      ( "arithmetic",
        [ Test.test_case "arithmetic expressions" `Quick test_arithmetic ] );
      ( "comparisons",
        [ Test.test_case "comparison expressions" `Quick test_comparisons ] );
      ("logical", [ Test.test_case "logical expressions" `Quick test_logical ]);
      ( "precedence",
        [ Test.test_case "operator precedence" `Quick test_precedence ] );
      ( "option_result",
        [
          Test.test_case "Option/Result constructors" `Quick test_option_result;
        ] );
      ( "conditionals",
        [ Test.test_case "if-then-else expressions" `Quick test_conditionals ]
      );
      ("tuples", [ Test.test_case "tuple expressions" `Quick test_tuples ]);
      ( "complex",
        [
          Test.test_case "complex nested expressions" `Quick
            test_complex_expressions;
        ] );
      ( "match_expressions",
        [ Test.test_case "match expressions" `Quick test_match_expressions ] );
      ( "let_expressions",
        [ Test.test_case "let expressions" `Quick test_let_expressions ] );
      ( "let_precedence",
        [
          Test.test_case "let with operator precedence" `Quick
            test_let_precedence;
        ] );
      ( "let_boundary_cases",
        [ Test.test_case "let boundary cases" `Quick test_let_boundary_cases ]
      );
      ( "let_edge_cases",
        [ Test.test_case "let edge cases" `Quick test_let_edge_cases ] );
      ( "function_expressions",
        [
          Test.test_case "function expressions" `Quick test_function_expressions;
        ] );
      ( "function_errors",
        [
          Test.test_case "function expression errors" `Quick
            test_function_errors;
        ] );
      ( "function_calls",
        [
          Test.test_case "function call expressions" `Quick test_function_calls;
        ] );
      ( "function_call_errors",
        [
          Test.test_case "function call errors" `Quick test_function_call_errors;
        ] );
      ( "array_access",
        [ Test.test_case "array access expressions" `Quick test_array_access ]
      );
      ( "array_access_errors",
        [ Test.test_case "array access errors" `Quick test_array_access_errors ]
      );
      ( "byte_literals",
        [ Test.test_case "byte literal expressions" `Quick test_byte_literals ]
      );
      ( "byte_literal_errors",
        [ Test.test_case "byte literal errors" `Quick test_byte_literal_errors ]
      );
      ( "package_system",
        [ Test.test_case "package system parsing" `Quick test_package_system ]
      );
      ( "package_system_errors",
        [
          Test.test_case "package system errors" `Quick
            test_package_system_errors;
        ] );
      ( "block_expressions",
        [ Test.test_case "block expressions" `Quick test_block_expressions ] );
      ( "block_expression_errors",
        [
          Test.test_case "block expression errors" `Quick
            test_block_expression_errors;
        ] );
      ( "array_literals",
        [
          Test.test_case "array literal expressions" `Quick test_array_literals;
        ] );
      ( "array_literal_errors",
        [
          Test.test_case "array literal errors" `Quick test_array_literal_errors;
        ] );
      ( "unary_operators",
        [ Test.test_case "unary operators" `Quick test_unary_operators ] );
      ( "tuple_errors",
        [ Test.test_case "tuple errors" `Quick test_tuple_errors ] );
      ( "chained_postfix",
        [
          Test.test_case "chained postfix expressions" `Quick
            test_chained_postfix;
        ] );
      ( "empty_param_functions",
        [
          Test.test_case "empty parameter functions" `Quick
            test_empty_param_functions;
        ] );
      ( "pattern_errors",
        [ Test.test_case "pattern matching errors" `Quick test_pattern_errors ]
      );
      ( "extreme_complexity",
        [
          Test.test_case "extremely complex expressions" `Quick
            test_extreme_complexity;
        ] );
      ( "precedence_edge_cases",
        [
          Test.test_case "operator precedence edge cases" `Quick
            test_precedence_edge_cases;
        ] );
      ( "error_recovery",
        [ Test.test_case "error recovery" `Quick test_error_recovery ] );
      ( "struct_literals",
        [ Test.test_case "struct literals" `Quick test_struct_literals ] );
      ( "struct_literal_errors",
        [
          Test.test_case "struct literal errors" `Quick
            test_struct_literal_errors;
        ] );
      ( "field_access",
        [ Test.test_case "field access" `Quick test_field_access ] );
      ( "field_access_errors",
        [ Test.test_case "field access errors" `Quick test_field_access_errors ]
      );
      ( "assignment_operators",
        [
          Test.test_case "assignment operators" `Quick test_assignment_operators;
        ] );
      ( "pipe_operators",
        [ Test.test_case "pipe operators" `Quick test_pipe_operators ] );
      ( "assignment_pipe_errors",
        [
          Test.test_case "assignment and pipe errors" `Quick
            test_assignment_pipe_errors;
        ] );
      ("while_loops", [ Test.test_case "while loops" `Quick test_while_loops ]);
      ("for_loops", [ Test.test_case "for loops" `Quick test_for_loops ]);
      ("loop_errors", [ Test.test_case "loop errors" `Quick test_loop_errors ]);
      ("errors", [ Test.test_case "error handling" `Quick test_errors ]);
      ( "match_errors",
        [ Test.test_case "match expression errors" `Quick test_match_errors ] );
      ( "let_errors",
        [ Test.test_case "let expression errors" `Quick test_let_errors ] );
      ( "comprehensive_errors",
        [
          Test.test_case "comprehensive error cases" `Quick
            test_comprehensive_errors;
        ] );
    ]
