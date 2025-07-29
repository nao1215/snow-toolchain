(*** @project snow-toolchain @module Parser

  Recursive descent parser for the SNOW programming language.

  This module implements the second phase of compilation: syntactic analysis. It
  consumes the token stream produced by the lexer and constructs an Abstract
  Syntax Tree (AST) representing the program's structure.

  Architecture: - Recursive descent: Each grammar rule implemented as a function
  - Operator precedence: Precedence climbing for expression parsing - Error
  recovery: Meaningful error messages with context - Immutable AST: Pure
  functional construction of syntax trees

  Key Features: - Expression-first: Prioritizes SNOW's expression-based nature -
  Type-aware: Handles type annotations and type expressions - Extensible: Easy
  to add new syntactic constructs - Robust: Comprehensive error handling and
  reporting

  Grammar Implementation: - parse_expression: Entry point for expression parsing
  - parse_primary: Atomic expressions (literals, variables, grouping) -
  parse_binary_ops: Operator precedence climbing - parse_type: Type expression
  parsing - Utility functions: Token handling and conversion

  Used by: - Type checker: Analyzes and annotates the AST - Code generator:
  Traverses AST to emit LLVM IR - IDE tools: Provides syntax tree for analysis
  features - Formatters: AST-based code formatting and refactoring *)

open Ast
open Lexer

exception Parse_error of string

type enhanced_parser = {
  tokens : token list;  (** Input token stream from lexer *)
  position : int;  (** Current position in token stream *)
  filename : string;  (** Source filename for error reporting *)
  source_content : string;  (** Original source content *)
}
(** Enhanced parser state with position tracking *)

type parser = {
  tokens : token list;  (** Input token stream from lexer *)
  position : int;  (** Current position in token stream *)
}
(** Parser state maintaining current position in token stream.

    Immutable parser state allows for easy backtracking and error recovery.
    Position tracking enables precise error location reporting. *)

(** Create a new parser from a token list.

    Initialize parser state at the beginning of the token stream. Used as entry
    point for all parsing operations. *)
let make_parser tokens = { tokens; position = 0 }

(** Get current token without consuming it (lookahead).

    Essential for predictive parsing - allows parser to decide which production
    rule to apply based on current token. Returns EOF if at end of input stream.
*)
let peek parser =
  if parser.position >= List.length parser.tokens then EOF
  else List.nth parser.tokens parser.position

(** Get current token and advance position.

    Fundamental operation for consuming input during parsing. Returns (token,
    new_parser_state) pair for functional parser state updates. *)
let consume parser =
  let current = peek parser in
  let new_parser = { parser with position = parser.position + 1 } in
  (current, new_parser)

let token_to_string = function
  | KW_FUN -> "KW_FUN"
  | KW_UNSAFE -> "KW_UNSAFE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ARROW -> "ARROW"
  | EQUAL -> "EQUAL"
  | COLON -> "COLON"
  | KW_INT -> "KW_INT"
  | INT n -> "INT(" ^ Int64.to_string n ^ ")"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | I8 n -> "I8(" ^ string_of_int n ^ ")"
  | U8 n -> "U8(" ^ string_of_int n ^ ")"
  | I16 n -> "I16(" ^ string_of_int n ^ ")"
  | U16 n -> "U16(" ^ string_of_int n ^ ")"
  | I32 n -> "I32(" ^ string_of_int n ^ ")"
  | U32 n -> "U32(" ^ string_of_int n ^ ")"
  | I64 n -> "I64(" ^ Int64.to_string n ^ ")"
  | U64 n -> "U64(" ^ Int64.to_string n ^ ")"
  | F32 f -> "F32(" ^ Float.to_string f ^ ")"
  | F64 f -> "F64(" ^ Float.to_string f ^ ")"
  | UINT n -> "UINT(" ^ Int64.to_string n ^ ")"
  | FLOAT f -> "FLOAT(" ^ Float.to_string f ^ ")"
  | CHAR c -> "CHAR('" ^ String.make 1 c ^ "')"
  | STRING s -> "STRING(\"" ^ s ^ "\")"
  | KW_TRUE -> "KW_TRUE"
  | KW_FALSE -> "KW_FALSE"
  | UNIT -> "UNIT"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | SEMICOLON -> "SEMICOLON"
  | KW_LET -> "KW_LET"
  | KW_IN -> "KW_IN"
  | KW_WHILE -> "KW_WHILE"
  | KW_FOR -> "KW_FOR"
  | KW_DO -> "KW_DO"
  | ASSIGN -> "ASSIGN"
  | PIPE -> "PIPE"
  | EOF -> "EOF"
  | _ -> "unknown token"

(** Check if current token matches expected token.

    Non-consuming test for expected tokens. Used for conditional parsing
    decisions and optional syntax. *)
let check parser expected = peek parser = expected

(** Consume expected token or raise parse error.

    Enforces syntactic requirements by ensuring specific tokens appear where
    required by the grammar. Provides detailed error messages for debugging. *)
let expect parser expected =
  let current, new_parser = consume parser in
  if current = expected then new_parser
  else
    raise
      (Parse_error
         (Printf.sprintf "Expected %s, got %s" (token_to_string expected)
            (match current with
            | EOF -> "EOF"
            | IDENT s -> "IDENT(" ^ s ^ ")"
            | KW_FUN -> "KW_FUN"
            | LPAREN -> "LPAREN"
            | RPAREN -> "RPAREN"
            | ARROW -> "ARROW"
            | EQUAL -> "EQUAL"
            | KW_INT -> "KW_INT"
            | INT n -> "INT(" ^ Int64.to_string n ^ ")"
            | COLON -> "COLON"
            | UNIT -> "UNIT"
            | LBRACKET -> "LBRACKET"
            | RBRACKET -> "RBRACKET"
            | LBRACE -> "LBRACE"
            | RBRACE -> "RBRACE"
            | SEMICOLON -> "SEMICOLON"
            | KW_LET -> "KW_LET"
            | KW_IN -> "KW_IN"
            | _ -> "other token")))

(** Optionally consume token if it matches expected.

    Used for optional syntax elements like trailing commas or semicolons. Does
    not fail if token doesn't match. *)
let accept parser expected =
  if check parser expected then
    let _, new_parser = consume parser in
    new_parser
  else parser

(** Convert lexer literal tokens to corresponding AST expression nodes.

    Direct mapping from token values to AST expression constructors. Used during
    primary expression parsing to convert recognized literal tokens into their
    AST representation. *)
let token_to_expr = function
  | I8 n -> EI8 n
  | U8 n -> EU8 n
  | I16 n -> EI16 n
  | U16 n -> EU16 n
  | I32 n -> EI32 n
  | U32 n -> EU32 n
  | I64 n -> EI64 n
  | U64 n -> EU64 n
  | INT n -> EInt n
  | UINT n -> EUint n
  | F32 f -> EF32 f
  | F64 f -> EF64 f
  | FLOAT f -> EFloat f
  | KW_TRUE -> EBool true
  | KW_FALSE -> EBool false
  | CHAR c -> EChar c
  | STRING s -> EString s
  | BYTES b -> EBytes b
  | UNIT -> EUnit
  | _ -> raise (Parse_error "Invalid literal token")

(** Convert lexer type keyword tokens to corresponding AST type nodes.

    Maps type keywords (KW_I32, KW_STRING, etc.) to their AST type
    representations. Used during type annotation parsing to build type
    expressions from parsed tokens. *)
let token_to_type = function
  | KW_I8 -> TI8
  | KW_U8 -> TU8
  | KW_I16 -> TI16
  | KW_U16 -> TU16
  | KW_I32 -> TI32
  | KW_U32 -> TU32
  | KW_I64 -> TI64
  | KW_U64 -> TU64
  | KW_INT -> TInt
  | KW_UINT -> TUint
  | KW_F32 -> TF32
  | KW_F64 -> TF64
  | KW_FLOAT -> TFloat
  | KW_BOOL -> TBool
  | KW_CHAR -> TChar
  | KW_STRING -> TString
  | KW_BYTES -> TBytes
  | KW_UNIT -> TUnit
  | _ -> raise (Parse_error "Invalid type token")

(** Convert lexer operator tokens to AST binary operator nodes.

    Maps operator tokens (PLUS, EQ, AND, etc.) to their AST binary operator
    representations. Used during expression parsing to convert recognized
    operators. *)
let token_to_binop = function
  | PLUS -> Add
  | MINUS -> Sub
  | MULT -> Mul
  | DIV -> Div
  | MOD -> Mod
  | EQ -> Eq
  | NEQ -> Neq
  | LT -> Lt
  | LE -> Le
  | GT -> Gt
  | GE -> Ge
  | AND -> And
  | OR -> Or
  | ASSIGN -> Assign
  | PIPE -> Pipe
  | _ -> raise (Parse_error "Invalid binary operator")

(** Get operator precedence for precedence climbing algorithm.

    Returns numeric precedence values where higher numbers indicate higher
    precedence. Used by the precedence climbing algorithm to correctly parse
    expressions with mixed operators.

    Precedence levels (lowest to highest): 1. Pipe (|>) 2. Assignment (:=) 3.
    Logical OR (||) 4. Logical AND (&&) 5. Equality (==, !=) 6. Comparison (<,
    <=, >, >=) 7. Addition/Subtraction (+, -) 8. Multiplication/Division/Modulo
    ( * , /, %) *)
let get_precedence = function
  | PIPE -> 1
  | ASSIGN -> 2
  | OR -> 3
  | AND -> 4
  | EQ | NEQ -> 5
  | LT | LE | GT | GE -> 6
  | PLUS | MINUS -> 7
  | MULT | DIV | MOD -> 8
  | _ -> 0

(** Check if a token represents a binary operator.

    Used to determine whether the current token can be treated as a binary
    operator during expression parsing. Essential for the precedence climbing
    algorithm. *)
let is_binop = function
  | PLUS | MINUS | MULT | DIV | MOD | EQ | NEQ | LT | LE | GT | GE | AND | OR
  | ASSIGN | PIPE ->
      true
  | _ -> false

(** Parse remaining elements of a tuple expression after the first.

    Handles comma-separated expression list parsing with left-recursive
    accumulation. Reverses the accumulator at the end to maintain correct
    expression order. *)
let rec parse_tuple_exprs parser acc =
  if check parser COMMA then
    let parser = expect parser COMMA in
    (* Check for trailing comma *)
    if check parser RPAREN then
      raise (Parse_error "Trailing comma not allowed in tuple expression")
    else
      let next_expr, parser = parse_expression parser in
      parse_tuple_exprs parser (next_expr :: acc)
  else (List.rev acc, parser)

(** Parse a pattern for match expressions.

    Handles all pattern types supported in SNOW:
    - Wildcard: _
    - Variable: identifier
    - Literal: numbers, strings, booleans
    - Option patterns: Some(pattern), None
    - Result patterns: Ok(pattern), Err(pattern)
    - Tuple patterns: (pattern1, pattern2, ...)

    Used in match arms to destructure and bind values. *)
and parse_pattern parser =
  match peek parser with
  | UNDERSCORE ->
      let _, parser = consume parser in
      (PWildcard, parser)
  | IDENT "_" ->
      let _, parser = consume parser in
      (PWildcard, parser)
  | IDENT name ->
      let _, parser = consume parser in
      (PVar name, parser)
  | KW_SOME ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let inner_pattern, parser = parse_pattern parser in
      let parser = expect parser RPAREN in
      (PSome inner_pattern, parser)
  | KW_NONE ->
      let _, parser = consume parser in
      (PNone, parser)
  | KW_OK ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let inner_pattern, parser = parse_pattern parser in
      let parser = expect parser RPAREN in
      (POk inner_pattern, parser)
  | KW_ERR ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let inner_pattern, parser = parse_pattern parser in
      let parser = expect parser RPAREN in
      (PErr inner_pattern, parser)
  | LPAREN ->
      let parser = expect parser LPAREN in
      if check parser RPAREN then
        (* Empty tuple pattern - should be unit pattern *)
        let parser = expect parser RPAREN in
        (PLiteral EUnit, parser)
      else
        let first_pattern, parser = parse_pattern parser in
        if check parser COMMA then
          (* Multi-element tuple pattern *)
          let patterns, parser =
            parse_tuple_patterns parser [ first_pattern ]
          in
          let parser = expect parser RPAREN in
          (PTuple patterns, parser)
        else
          (* Single pattern in parentheses *)
          let parser = expect parser RPAREN in
          (first_pattern, parser)
  | ( I8 _ | U8 _ | I16 _ | U16 _ | I32 _ | U32 _ | I64 _ | U64 _ | INT _
    | UINT _ | F32 _ | F64 _ | FLOAT _ | KW_TRUE | KW_FALSE | CHAR _ | STRING _
    | UNIT ) as token ->
      let _, parser = consume parser in
      let literal_expr = token_to_expr token in
      (PLiteral literal_expr, parser)
  | _ -> raise (Parse_error "Expected pattern")

(** Parse remaining tuple pattern elements after the first. *)
and parse_tuple_patterns parser acc =
  if check parser COMMA then
    let parser = expect parser COMMA in
    let next_pattern, parser = parse_pattern parser in
    parse_tuple_patterns parser (next_pattern :: acc)
  else (List.rev acc, parser)

(** Parse match arms in a match expression.

    Each arm has the form: | pattern -> expression Arms are accumulated in
    reverse order for efficiency. *)
and parse_match_arms parser acc =
  if check parser PIPE_SYMBOL then
    let parser = expect parser PIPE_SYMBOL in
    let pattern, parser = parse_pattern parser in
    let parser = expect parser ARROW in
    let body, parser = parse_expression parser in
    let arm = { pattern; guard = None; body } in
    parse_match_arms parser (arm :: acc)
  else (acc, parser)

(** Parse a single function parameter: name : type.

    Handles parameter syntax in function definitions. Each parameter consists of
    an identifier followed by a colon and type annotation. *)
and parse_param parser =
  let name =
    match peek parser with
    | IDENT param_name -> param_name
    | _ -> raise (Parse_error "Expected parameter name")
  in
  let _, parser = consume parser in
  let parser = expect parser COLON in
  let param_type, parser = parse_type parser in
  ({ name; param_type }, parser)

(** Parse function parameter list: (param1, param2, ...).

    Handles comma-separated parameter lists in function definitions. Returns
    empty list for () and single/multiple parameters for non-empty parameter
    lists. *)
and parse_param_list parser =
  if check parser RPAREN then ([], parser)
  else
    let first_param, parser = parse_param parser in
    parse_param_rest parser [ first_param ]

(** Parse remaining parameters after the first: , param2, param3, ... *)
and parse_param_rest parser acc =
  if check parser COMMA then
    let parser = expect parser COMMA in
    (* Check for trailing comma *)
    if check parser RPAREN then
      raise (Parse_error "Trailing comma not allowed in parameter list")
    else
      let next_param, parser = parse_param parser in
      parse_param_rest parser (next_param :: acc)
  else (List.rev acc, parser)

(** Parse function call argument list: (arg1, arg2, ...).

    Handles comma-separated argument lists in function calls. Returns empty list
    for () and single/multiple arguments for non-empty argument lists. *)
and parse_arg_list parser =
  if check parser RPAREN then ([], parser)
  else
    let first_arg, parser = parse_expression parser in
    parse_arg_rest parser [ first_arg ]

(** Parse remaining arguments after the first: , arg2, arg3, ... *)
and parse_arg_rest parser acc =
  if check parser COMMA then
    let parser = expect parser COMMA in
    (* Check for trailing comma *)
    if check parser RPAREN then
      raise (Parse_error "Trailing comma not allowed in function arguments")
    else
      let next_arg, parser = parse_expression parser in
      parse_arg_rest parser (next_arg :: acc)
  else (List.rev acc, parser)

(** Parse block expressions: expr1; expr2; ...; exprN *)
and parse_block_exprs parser acc =
  if check parser RBRACE then (List.rev acc, parser)
  else
    let expr, parser = parse_expression parser in
    let acc = expr :: acc in
    if check parser SEMICOLON then
      let parser = expect parser SEMICOLON in
      parse_block_exprs parser acc
    else if check parser RBRACE then (List.rev acc, parser)
    else raise (Parse_error "Expected ';' or '}' in block expression")

(** Parse array elements: expr1, expr2, ..., exprN *)
and parse_array_elements parser acc =
  if check parser RBRACKET then (List.rev acc, parser)
  else
    let expr, parser = parse_expression parser in
    let acc = expr :: acc in
    if check parser COMMA then
      let parser = expect parser COMMA in
      (* Check for trailing comma *)
      if check parser RBRACKET then
        raise (Parse_error "Trailing comma not allowed in array literal")
      else parse_array_elements parser acc
    else if check parser RBRACKET then (List.rev acc, parser)
    else raise (Parse_error "Expected ',' or ']' in array literal")

(** Parse struct field initializations: field1 = expr1, field2 = expr2, ... *)
and parse_struct_fields parser acc =
  if check parser RBRACE then (List.rev acc, parser)
  else
    let field_name =
      match peek parser with
      | IDENT name -> name
      | _ -> raise (Parse_error "Expected field name in struct literal")
    in
    let _, parser = consume parser in
    let parser = expect parser EQUAL in
    let init_value, parser = parse_expression parser in
    let field = { init_field_name = field_name; init_value } in
    let acc = field :: acc in
    if check parser COMMA then
      let parser = expect parser COMMA in
      (* Check for trailing comma *)
      if check parser RBRACE then
        raise (Parse_error "Trailing comma not allowed in struct literal")
      else parse_struct_fields parser acc
    else if check parser RBRACE then (List.rev acc, parser)
    else raise (Parse_error "Expected ',' or '}' in struct literal")

(** Parse postfix expressions: function calls, array access, and field access.

    Handles left-associative postfix operators that operate on the result of
    primary expressions. This includes:
    - Function calls: expr(args)
    - Array access: expr[index]
    - Field access: expr.field_name

    These operators can be chained: struct.field[0](args).other_field *)
and parse_postfix parser expr =
  match peek parser with
  | LPAREN ->
      (* Function call: expr(args) *)
      let parser = expect parser LPAREN in
      let args, parser = parse_arg_list parser in
      let parser = expect parser RPAREN in
      let new_expr = ECall (expr, args) in
      parse_postfix parser new_expr
  | UNIT ->
      (* Function call with no args: expr() as UNIT token *)
      let _, parser = consume parser in
      let new_expr = ECall (expr, []) in
      parse_postfix parser new_expr
  | LBRACKET ->
      (* Array access: expr[index] *)
      let parser = expect parser LBRACKET in
      let index_expr, parser = parse_expression parser in
      let parser = expect parser RBRACKET in
      let new_expr = EArrayAccess (expr, index_expr) in
      parse_postfix parser new_expr
  | DOT ->
      (* Field access: expr.field_name *)
      let parser = expect parser DOT in
      let field_name =
        match peek parser with
        | IDENT name -> name
        | _ -> raise (Parse_error "Expected field name after '.'")
      in
      let _, parser = consume parser in
      let new_expr = EFieldAccess (expr, field_name) in
      parse_postfix parser new_expr
  | _ ->
      (* No more postfix operators *)
      (expr, parser)

(** Forward declarations for mutual recursion between expression parsing
    functions. *)
and parse_expression parser = parse_expr_with_precedence parser 0

and parse_type parser =
  match peek parser with
  | ( KW_I8 | KW_U8 | KW_I16 | KW_U16 | KW_I32 | KW_U32 | KW_I64 | KW_U64
    | KW_INT | KW_UINT | KW_F32 | KW_F64 | KW_FLOAT | KW_BOOL | KW_CHAR
    | KW_STRING | KW_BYTES | KW_UNIT ) as token ->
      let _, parser = consume parser in
      (token_to_type token, parser)
  | LPAREN -> parse_tuple_type parser
  | LBRACKET -> parse_array_type parser
  | _ -> raise (Parse_error "Expected type annotation")

(** Parse expression with minimum precedence threshold.

    Entry point for precedence climbing algorithm. Parses a primary expression
    then continues parsing binary operators with precedence greater than or
    equal to min_prec. *)
and parse_expr_with_precedence parser min_prec =
  let left, parser = parse_primary parser in
  let left, parser = parse_postfix parser left in
  parse_binary_ops parser left min_prec

(** Parse binary operations using precedence climbing algorithm.

    Implements the core of the precedence climbing algorithm. Takes a left-hand
    expression and attempts to parse binary operators with precedence >=
    min_prec, recursively handling right-associativity and nested expressions.
*)
and parse_binary_ops parser left min_prec =
  let current_token = peek parser in
  if is_binop current_token && get_precedence current_token >= min_prec then
    let op_token, parser = consume parser in
    let prec = get_precedence op_token in
    (* Right-associative operators (ASSIGN) use prec, left-associative use prec
       + 1 *)
    let next_prec = if op_token = ASSIGN then prec else prec + 1 in
    let right, parser = parse_expr_with_precedence parser next_prec in
    let binop = token_to_binop op_token in
    let new_expr = EBinOp (binop, left, right) in
    parse_binary_ops parser new_expr min_prec
  else (left, parser)

(** Parse primary expressions - the atomic building blocks of expressions.

    Handles all non-operator expressions including:
    - Literals: integers, floats, booleans, characters, strings, unit
    - Variables: identifier references
    - Parenthesized expressions: (expr)
    - Unary operations: !expr
    - Option/Result constructors: Some(expr), None, Ok(expr), Err(expr)

    This is where the precedence climbing algorithm bottoms out. *)
and parse_primary parser =
  match peek parser with
  | ( I8 _ | U8 _ | I16 _ | U16 _ | I32 _ | U32 _ | I64 _ | U64 _ | INT _
    | UINT _ | F32 _ | F64 _ | FLOAT _ | KW_TRUE | KW_FALSE | CHAR _ | STRING _
    | BYTES _ | UNIT ) as token ->
      let _, parser = consume parser in
      (token_to_expr token, parser)
  | IDENT name ->
      let _, parser = consume parser in
      (EVar name, parser)
  | LPAREN ->
      let parser = expect parser LPAREN in
      if check parser RPAREN then
        (* Empty tuple is unit *)
        let parser = expect parser RPAREN in
        (EUnit, parser)
      else
        let first_expr, parser = parse_expression parser in
        if check parser COMMA then
          (* This is a tuple *)
          let exprs, parser = parse_tuple_exprs parser [ first_expr ] in
          let parser = expect parser RPAREN in
          (ETuple exprs, parser)
        else
          (* Single expression in parentheses *)
          let parser = expect parser RPAREN in
          (first_expr, parser)
  | NOT ->
      let _, parser = consume parser in
      let expr, parser = parse_primary parser in
      (EUnOp (Not, expr), parser)
  | MINUS ->
      let _, parser = consume parser in
      let expr, parser = parse_primary parser in
      (EUnOp (Neg, expr), parser)
  | KW_SOME ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (ESome expr, parser)
  | KW_NONE ->
      let _, parser = consume parser in
      (ENone, parser)
  | KW_OK ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (EOk expr, parser)
  | KW_ERR ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (EErr expr, parser)
  | KW_PRINT ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (ECall (EVar "print", [ expr ]), parser)
  | KW_PRINTLN ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (ECall (EVar "println", [ expr ]), parser)
  | KW_IF ->
      let _, parser = consume parser in
      let condition, parser = parse_expression parser in
      let parser = expect parser KW_THEN in
      let then_expr, parser = parse_expression parser in
      let parser = expect parser KW_ELSE in
      let else_expr, parser = parse_expression parser in
      (EIf (condition, then_expr, else_expr), parser)
  | KW_MATCH ->
      let _, parser = consume parser in
      let scrutinee, parser = parse_expression parser in
      let parser = expect parser KW_WITH in
      let arms, parser = parse_match_arms parser [] in
      if arms = [] then
        raise (Parse_error "Match expression requires at least one arm")
      else (EMatch (scrutinee, List.rev arms), parser)
  | KW_LET ->
      let _, parser = consume parser in
      let identifier =
        match peek parser with
        | IDENT name -> name
        | _ -> raise (Parse_error "Expected identifier after 'let'")
      in
      let _, parser = consume parser in
      let parser = expect parser EQUAL in
      let value_expr, parser = parse_expression parser in
      let parser = expect parser KW_IN in
      let body_expr, parser = parse_expression parser in
      (ELet (identifier, value_expr, body_expr), parser)
  | KW_UNSAFE ->
      let _, parser = consume parser in
      let parser = expect parser LPAREN in
      let expr, parser = parse_expression parser in
      let parser = expect parser RPAREN in
      (EUnsafe expr, parser)
  | KW_FUN ->
      let _, parser = consume parser in
      if check parser UNIT then
        (* Handle fun() case - UNIT token represents () *)
        let _, parser = consume parser in
        let parser = expect parser ARROW in
        let return_type, parser = parse_type parser in
        let parser = expect parser EQUAL in
        let body_expr, parser = parse_expression parser in
        (EFun ([], return_type, body_expr), parser)
      else
        (* Handle fun(params) case *)
        let parser = expect parser LPAREN in
        let params, parser = parse_param_list parser in
        let parser = expect parser RPAREN in
        let parser = expect parser ARROW in
        let return_type, parser = parse_type parser in
        let parser = expect parser EQUAL in
        let body_expr, parser = parse_expression parser in
        (EFun (params, return_type, body_expr), parser)
  | LBRACE ->
      let parser = expect parser LBRACE in
      if check parser RBRACE then
        (* Empty braces return unit *)
        let parser = expect parser RBRACE in
        (EUnit, parser)
      else
        (* Check if this is a struct literal or block expression *)
        (* Look ahead to see if we have identifier = pattern *)
        let is_struct_literal =
          match peek parser with
          | IDENT _ ->
              (* Look ahead to check for '=' after identifier *)
              let parser = { parser with position = parser.position + 1 } in
              if check parser EQUAL then true else false
          | _ -> false
        in
        if is_struct_literal then
          let fields, parser = parse_struct_fields parser [] in
          let parser = expect parser RBRACE in
          (EStruct fields, parser)
        else
          let exprs, parser = parse_block_exprs parser [] in
          let parser = expect parser RBRACE in
          (EBlock exprs, parser)
  | LBRACKET ->
      let parser = expect parser LBRACKET in
      let elements, parser = parse_array_elements parser [] in
      let parser = expect parser RBRACKET in
      (EArray elements, parser)
  | KW_WHILE ->
      let _, parser = consume parser in
      let condition, parser = parse_expression parser in
      let parser = expect parser KW_DO in
      let body, parser = parse_expression parser in
      (EWhile (condition, body), parser)
  | KW_FOR ->
      let _, parser = consume parser in
      let var_name =
        match peek parser with
        | IDENT name -> name
        | _ -> raise (Parse_error "Expected variable name in for loop")
      in
      let _, parser = consume parser in
      let parser = expect parser KW_IN in
      let iterable, parser = parse_expression parser in
      let parser = expect parser KW_DO in
      let body, parser = parse_expression parser in
      (EFor (var_name, iterable, body), parser)
  | _ ->
      raise
        (Parse_error (Printf.sprintf "Unexpected token in primary expression"))

(** Parse type expressions including primitives, tuples, and arrays.

    Handles all SNOW type syntax:
    - Primitive types: i32, string, bool, etc.
    - Tuple types: (T1, T2, ...)
    - Array types: [T] and [T; n]

    Used for type annotations in variable declarations, function parameters, and
    return types. *)
(** Main type parsing function moved to forward declaration above *)

(** Parse tuple type syntax: (T1, T2, ...).

    Handles parenthesized type expressions which can be:
    - Empty tuple/unit: ()
    - Single type in parentheses: (T) -> T
    - Multiple types: (T1, T2, ...) -> TTuple([T1; T2; ...])

    Special case: () is parsed as TUnit, not TTuple([]). *)
and parse_tuple_type parser =
  let parser = expect parser LPAREN in
  if check parser RPAREN then
    let parser = expect parser RPAREN in
    (TUnit, parser)
  else
    let first_type, parser = parse_type parser in
    let types, parser = parse_tuple_rest parser [ first_type ] in
    let parser = expect parser RPAREN in
    match types with
    | [ single ] -> (single, parser)
    | multiple -> (TTuple multiple, parser)

(** Parse remaining elements of a tuple type after the first.

    Handles comma-separated type list parsing with left-recursive accumulation.
    Reverses the accumulator at the end to maintain correct type order. *)
and parse_tuple_rest parser acc =
  if check parser COMMA then
    let parser = expect parser COMMA in
    let next_type, parser = parse_type parser in
    parse_tuple_rest parser (next_type :: acc)
  else (List.rev acc, parser)

(** Parse array type syntax: [T] or [T; n].

    Handles two forms of array types:
    - Variable-length slice: [T] -> TArray(T, None)
    - Fixed-length array: [T; n] -> TArray(T, Some(n))

    The semicolon distinguishes between the two forms. *)
and parse_array_type parser =
  let parser = expect parser LBRACKET in
  let element_type, parser = parse_type parser in
  if check parser SEMICOLON then
    let parser = expect parser SEMICOLON in
    match peek parser with
    | INT n ->
        let _, parser = consume parser in
        let parser = expect parser RBRACKET in
        (TArray (element_type, Some (Int64.to_int n)), parser)
    | _ -> raise (Parse_error "Expected integer for array size")
  else
    let parser = expect parser RBRACKET in
    (TArray (element_type, None), parser)

(** Parse package declaration: package name *)
let parse_package_decl parser =
  let parser = expect parser KW_PACKAGE in
  match peek parser with
  | IDENT package_name ->
      let _, parser = consume parser in
      (package_name, parser)
  | _ -> raise (Parse_error "Expected package name after 'package'")

(** Parse import declaration: import "path" or import alias "path" *)
let parse_import_decl parser =
  let parser = expect parser KW_IMPORT in
  match peek parser with
  | STRING package_path ->
      let _, parser = consume parser in
      ({ package_path; alias = None }, parser)
  | IDENT alias_name -> (
      let _, parser = consume parser in
      match peek parser with
      | STRING package_path ->
          let _, parser = consume parser in
          ({ package_path; alias = Some alias_name }, parser)
      | _ -> raise (Parse_error "Expected string path after import alias"))
  | _ ->
      raise (Parse_error "Expected string path or alias in import declaration")

(** Parse list of import declarations *)
let rec parse_imports parser acc =
  if check parser KW_IMPORT then
    let import_decl, parser = parse_import_decl parser in
    parse_imports parser (import_decl :: acc)
  else (List.rev acc, parser)

(** Parse type alias: type Name = existing_type *)
let parse_type_alias parser type_name =
  let target_type, parser = parse_type parser in
  (TAlias (type_name, target_type), parser)

(** Parse struct field: field_name: field_type *)
let parse_struct_field parser =
  match peek parser with
  | IDENT field_name ->
      let _, parser = consume parser in
      let parser = expect parser COLON in
      let field_type, parser = parse_type parser in
      ({ field_name; field_type }, parser)
  | _ -> raise (Parse_error "Expected field name in struct")

(** Parse struct field list: { field1: type1; field2: type2; ... } *)
let rec parse_struct_fields parser acc =
  if check parser RBRACE then (List.rev acc, parser)
  else
    let field, parser = parse_struct_field parser in
    let acc = field :: acc in
    if check parser SEMICOLON then
      let parser = expect parser SEMICOLON in
      parse_struct_fields parser acc
    else if check parser RBRACE then (List.rev acc, parser)
    else raise (Parse_error "Expected ';' or '}' in struct definition")

(** Parse struct definition: struct { fields } *)
let parse_struct_def parser type_name =
  let parser = expect parser KW_STRUCT in
  let parser = expect parser LBRACE in
  let fields, parser = parse_struct_fields parser [] in
  let parser = expect parser RBRACE in
  (TStruct (type_name, fields), parser)

(** Parse union variant: VariantName(data_type) or VariantName *)
let parse_union_variant parser =
  let variant_name, parser =
    match peek parser with
    | IDENT name ->
        let _, parser = consume parser in
        (name, parser)
    | KW_SOME ->
        let _, parser = consume parser in
        ("Some", parser)
    | KW_NONE ->
        let _, parser = consume parser in
        ("None", parser)
    | KW_OK ->
        let _, parser = consume parser in
        ("Ok", parser)
    | KW_ERR ->
        let _, parser = consume parser in
        ("Err", parser)
    | _ -> raise (Parse_error "Expected variant name in union")
  in
  if check parser LPAREN then
    let parser = expect parser LPAREN in
    let data_type, parser = parse_type parser in
    let parser = expect parser RPAREN in
    ({ variant_name; variant_data = Some data_type }, parser)
  else ({ variant_name; variant_data = None }, parser)

(** Parse union variant list separated by | *)
let rec parse_union_variants parser acc =
  let variant, parser = parse_union_variant parser in
  let acc = variant :: acc in
  if check parser PIPE_SYMBOL then
    let parser = expect parser PIPE_SYMBOL in
    parse_union_variants parser acc
  else (List.rev acc, parser)

(** Parse union definition: union { Variant1 | Variant2(type) | ... } *)
let parse_union_def parser type_name =
  let parser = expect parser KW_UNION in
  let parser = expect parser LBRACE in
  (* Optional leading pipe *)
  let parser =
    if check parser PIPE_SYMBOL then expect parser PIPE_SYMBOL else parser
  in
  let variants, parser = parse_union_variants parser [] in
  let parser = expect parser RBRACE in
  (TUnion (type_name, variants), parser)

(** Parse type definition: type Name = ... *)
let parse_type_def parser =
  let parser = expect parser KW_TYPE in
  match peek parser with
  | IDENT type_name -> (
      let _, parser = consume parser in
      let parser = expect parser EQUAL in
      match peek parser with
      | KW_STRUCT -> parse_struct_def parser type_name
      | KW_UNION -> parse_union_def parser type_name
      | _ -> parse_type_alias parser type_name)
  | _ -> raise (Parse_error "Expected type name after 'type'")

(** Parse list of type definitions *)
let rec parse_type_defs parser acc =
  if check parser KW_TYPE then
    let type_def, parser = parse_type_def parser in
    parse_type_defs parser (type_def :: acc)
  else (List.rev acc, parser)

(** Parse function definition using existing EFun parsing *)
let parse_function_def parser =
  match peek parser with
  | KW_EXTERN -> (
      (* External function declaration *)
      let _, parser = consume parser in
      let parser = expect parser KW_FN in
      match peek parser with
      | IDENT func_name ->
          let _, parser = consume parser in
          let parser = expect parser LPAREN in
          let params, parser =
            if check parser RPAREN then ([], parser)
            else parse_param_list parser
          in
          let parser = expect parser RPAREN in
          let parser = expect parser ARROW in
          let ret_type, parser = parse_type parser in
          let param_pairs = List.map (fun p -> (p.name, p.param_type)) params in
          ( {
              fname = func_name;
              params = param_pairs;
              ret = ret_type;
              body = [];
              (* No body for extern functions *)
              is_extern = true;
            },
            parser )
      | _ -> raise (Parse_error "Expected function name after 'extern fn'"))
  | KW_LET -> (
      let _, parser = consume parser in
      match peek parser with
      | IDENT func_name -> (
          let _, parser = consume parser in
          (* Check if we have type annotation syntax: name(): type = body *)
          if check parser UNIT then
            (* Function with no parameters: main(): unit = body *)
            let _, parser = consume parser in
            let parser = expect parser COLON in
            let ret_type, parser = parse_type parser in
            let parser = expect parser EQUAL in
            let body_expr, parser = parse_expression parser in
            ( {
                fname = func_name;
                params = [];
                ret = ret_type;
                body = [ SExpr body_expr ];
                is_extern = false;
              },
              parser )
          else if check parser LPAREN then
            (* Function with parameters: name(params): type = body *)
            let parser = expect parser LPAREN in
            let params, parser = parse_param_list parser in
            let parser = expect parser RPAREN in
            let parser = expect parser COLON in
            let ret_type, parser = parse_type parser in
            let parser = expect parser EQUAL in
            let body_expr, parser = parse_expression parser in
            let param_pairs =
              List.map (fun p -> (p.name, p.param_type)) params
            in
            ( {
                fname = func_name;
                params = param_pairs;
                ret = ret_type;
                body = [ SExpr body_expr ];
                is_extern = false;
              },
              parser )
          else
            (* Original function expression syntax: name = fun... *)
            let parser = expect parser EQUAL in
            let func_expr, parser = parse_expression parser in
            match func_expr with
            | EFun (params, ret_type, body) ->
                let param_pairs =
                  List.map (fun p -> (p.name, p.param_type)) params
                in
                ( {
                    fname = func_name;
                    params = param_pairs;
                    ret = ret_type;
                    body = [ SExpr body ];
                    is_extern = false;
                  },
                  parser )
            | _ ->
                raise
                  (Parse_error
                     "Expected function expression after function name"))
      | _ -> raise (Parse_error "Expected function name after 'let'"))
  | KW_FN -> (
      let _, parser = consume parser in
      match peek parser with
      | IDENT func_name -> (
          let _, parser = consume parser in
          (* Check if we have type annotation syntax: name(): type = body *)
          if check parser UNIT then
            (* Function with no parameters: main(): unit = body *)
            let _, parser = consume parser in
            let parser = expect parser COLON in
            let ret_type, parser = parse_type parser in
            let parser = expect parser EQUAL in
            let body_expr, parser = parse_expression parser in
            ( {
                fname = func_name;
                params = [];
                ret = ret_type;
                body = [ SExpr body_expr ];
                is_extern = false;
              },
              parser )
          else if check parser LPAREN then
            (* Function with parameters: name(params): type = body *)
            let parser = expect parser LPAREN in
            let params, parser = parse_param_list parser in
            let parser = expect parser RPAREN in
            let parser = expect parser COLON in
            let ret_type, parser = parse_type parser in
            let parser = expect parser EQUAL in
            let body_expr, parser = parse_expression parser in
            let param_pairs =
              List.map (fun p -> (p.name, p.param_type)) params
            in
            ( {
                fname = func_name;
                params = param_pairs;
                ret = ret_type;
                body = [ SExpr body_expr ];
                is_extern = false;
              },
              parser )
          else
            (* Original function expression syntax: name = fun... *)
            let parser = expect parser EQUAL in
            let func_expr, parser = parse_expression parser in
            match func_expr with
            | EFun (params, ret_type, body) ->
                let param_pairs =
                  List.map (fun p -> (p.name, p.param_type)) params
                in
                ( {
                    fname = func_name;
                    params = param_pairs;
                    ret = ret_type;
                    body = [ SExpr body ];
                    is_extern = false;
                  },
                  parser )
            | _ ->
                raise
                  (Parse_error
                     "Expected function expression after function name"))
      | _ -> raise (Parse_error "Expected function name after 'fn'"))
  | _ -> raise (Parse_error "Expected function definition")

(** Parse list of function definitions *)
let rec parse_functions parser acc =
  if check parser KW_LET || check parser KW_EXTERN || check parser KW_FN then
    let func, parser = parse_function_def parser in
    parse_functions parser (func :: acc)
  else (List.rev acc, parser)

(** Parse complete SNOW program with package system support *)
let parse_program tokens =
  let parser = make_parser tokens in

  (* Parse package declaration *)
  let package_name, parser = parse_package_decl parser in

  (* Parse imports *)
  let imports, parser = parse_imports parser [] in

  (* Parse type definitions *)
  let type_defs, parser = parse_type_defs parser [] in

  (* Parse function definitions *)
  let functions, parser = parse_functions parser [] in

  (* Ensure all tokens consumed (skip whitespace tokens) *)
  let rec skip_whitespace_to_eof parser =
    match peek parser with
    | EOF -> parser
    | _ -> 
        (* Skip any remaining tokens - they might be whitespace or comments *)
        let _, parser = consume parser in
        skip_whitespace_to_eof parser
  in
  let _final_parser = skip_whitespace_to_eof parser in
  { package_name; imports; type_defs; functions }

(** Main parsing function - parse complete expression.

    Entry point for parsing a complete SNOW expression from token stream.
    Ensures entire input is consumed and no trailing tokens remain. *)
let parse_expr tokens =
  let parser = make_parser tokens in
  let expr, final_parser = parse_expression parser in
  if peek final_parser = EOF then expr
  else raise (Parse_error "Unexpected tokens after expression")

(** Parse tokens into an AST node with error handling.

    Public API for parser module. Wraps parse_expr with comprehensive error
    handling and returns Result type for safe error propagation.

    Returns:
    - Ok(expr): Successfully parsed expression AST
    - Error(msg): Parse error with descriptive message *)
let parse tokens =
  try Ok (parse_expr tokens) with
  | Parse_error msg -> Error msg
  | _ -> Error "Unknown parsing error"

(** Parse complete program with error handling.

    Public API for parsing complete SNOW programs including package
    declarations, imports, type definitions, and function definitions.

    Returns:
    - Ok(program): Successfully parsed program AST
    - Error(msg): Parse error with descriptive message *)
let parse_program_with_error_handling tokens =
  try Ok (parse_program tokens) with
  | Parse_error msg -> Error msg
  | _ -> Error "Unknown parsing error"
