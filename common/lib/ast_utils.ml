(*** @project snow-toolchain @module AST_Utils

  Utility functions for working with AST nodes in the SNOW programming language.

  This module provides common operations on AST nodes that are used across
  multiple components of the compiler. By centralizing these utilities, we
  reduce code duplication and ensure consistency across the codebase.

  Design Philosophy: - Single source of truth for AST operations - Pure
  functions with no side effects - Comprehensive error handling with descriptive
  messages - Easy to test and maintain

  Used by: - Type checker: For type inference operations - Code generator: For
  literal value conversion - Semantic analyzer: For AST validation - Formatter:
  For AST pretty-printing *)

open Ast
open Printf

exception Ast_util_error of string
(** Exception type for AST utility errors *)

(** Literal type inference utilities.

    This section provides functions for inferring types from literal
    expressions. These functions are used by both the basic and advanced type
    checkers to avoid code duplication. *)

(** Infer the type of a literal expression.

    @param expr The expression to infer the type for
    @return The inferred type
    @raise Ast_util_error if the expression is not a literal *)
let infer_literal_type = function
  | EI8 _ -> TI8
  | EU8 _ -> TU8
  | EI16 _ -> TI16
  | EU16 _ -> TU16
  | EI32 _ -> TI32
  | EU32 _ -> TU32
  | EI64 _ -> TI64
  | EU64 _ -> TU64
  | EInt _ -> TInt
  | EUint _ -> TUint
  | EF32 _ -> TF32
  | EF64 _ -> TF64
  | EFloat _ -> TFloat
  | EBool _ -> TBool
  | EChar _ -> TChar
  | EString _ -> TString
  | EBytes _ -> TBytes
  | EUnit -> TUnit
  | _ -> raise (Ast_util_error "Expression is not a literal")

(** Check if an expression is a literal.

    @param expr The expression to check
    @return true if the expression is a literal, false otherwise *)
let is_literal = function
  | EI8 _ | EU8 _ | EI16 _ | EU16 _ | EI32 _ | EU32 _ | EI64 _ | EU64 _ | EInt _
  | EUint _ | EF32 _ | EF64 _ | EFloat _ | EBool _ | EChar _ | EString _
  | EBytes _ | EUnit ->
      true
  | _ -> false

(** Get the string representation of a type for error messages.

    @param typ The type to convert to string
    @return String representation of the type *)
let rec type_to_string = function
  | TVar n -> sprintf "TVar(%d)" n
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
  | TOption typ -> sprintf "Option<%s>" (type_to_string typ)
  | TResult (ok_typ, err_typ) ->
      sprintf "Result<%s, %s>" (type_to_string ok_typ) (type_to_string err_typ)
  | TTuple types ->
      sprintf "(%s)" (String.concat ", " (List.map type_to_string types))
  | TArray (typ, Some size) -> sprintf "[%s; %d]" (type_to_string typ) size
  | TArray (typ, None) -> sprintf "[%s]" (type_to_string typ)
  | TFunc (param_types, return_type) ->
      sprintf "fn(%s) -> %s"
        (String.concat ", " (List.map type_to_string param_types))
        (type_to_string return_type)
  | TStruct fields ->
      sprintf "struct { %s }"
        (String.concat "; "
           (List.map
              (fun f ->
                sprintf "%s: %s" f.field_name (type_to_string f.field_type))
              fields))
  | TUnion (name, _) -> sprintf "union %s" name

(** Get the string representation of an expression for error messages.

    @param expr The expression to convert to string
    @return String representation of the expression *)
let expr_to_string = function
  | EI8 n -> sprintf "%di8" n
  | EU8 n -> sprintf "%du8" n
  | EI16 n -> sprintf "%di16" n
  | EU16 n -> sprintf "%du16" n
  | EI32 n -> sprintf "%di32" n
  | EU32 n -> sprintf "%du32" n
  | EI64 n -> sprintf "%Ldi64" n
  | EU64 n -> sprintf "%Ldu64" n
  | EInt n -> sprintf "%Ld" n
  | EUint n -> sprintf "%Ldu" n
  | EF32 f -> sprintf "%gf32" f
  | EF64 f -> sprintf "%gf64" f
  | EFloat f -> sprintf "%g" f
  | EBool true -> "true"
  | EBool false -> "false"
  | EChar c -> sprintf "'%c'" c
  | EString s -> sprintf "\"%s\"" s
  | EBytes _ -> "bytes"
  | EUnit -> "()"
  | EVar name -> name
  | EBinOp (op, _, _) ->
      sprintf "binary_%s"
        (match op with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
        | Mod -> "mod"
        | Eq -> "eq"
        | Neq -> "neq"
        | Lt -> "lt"
        | Le -> "le"
        | Gt -> "gt"
        | Ge -> "ge"
        | And -> "and"
        | Or -> "or"
        | Assign -> "assign"
        | Pipe -> "pipe")
  | EUnOp (op, _) ->
      sprintf "unary_%s" (match op with Not -> "not" | Neg -> "neg")
  | ECall (_, _) -> "function_call"
  | ELet (_, _, _) -> "let_expression"
  | EIf (_, _, _) -> "if_expression"
  | EMatch (_, _) -> "match_expression"
  | ETuple _ -> "tuple"
  | EArray _ -> "array"
  | EArrayAccess (_, _) -> "array_access"
  | EFieldAccess (_, _) -> "field_access"
  | EBlock _ -> "block"
  | EWhile (_, _) -> "while_loop"
  | EFor (_, _, _) -> "for_loop"
  | EFun (_, _, _) -> "function_expression"
  | EStruct _ -> "struct_literal"
  | ESome _ -> "some"
  | ENone -> "none"
  | EOk _ -> "ok"
  | EErr _ -> "err"
  | EPrint _ -> "print"
  | EPrintln _ -> "println"
  | EUnsafe _ -> "unsafe"

(** Check if two types are equivalent.

    @param t1 The first type
    @param t2 The second type
    @return true if types are equivalent, false otherwise *)
let rec types_equal t1 t2 =
  match (t1, t2) with
  | TVar n1, TVar n2 -> n1 = n2
  | TI8, TI8
  | TU8, TU8
  | TI16, TI16
  | TU16, TU16
  | TI32, TI32
  | TU32, TU32
  | TI64, TI64
  | TU64, TU64
  | TInt, TInt
  | TUint, TUint
  | TF32, TF32
  | TF64, TF64
  | TFloat, TFloat
  | TBool, TBool
  | TChar, TChar
  | TString, TString
  | TBytes, TBytes
  | TUnit, TUnit
  | TNever, TNever ->
      true
  | TOption t1, TOption t2 -> types_equal t1 t2
  | TResult (ok1, err1), TResult (ok2, err2) ->
      types_equal ok1 ok2 && types_equal err1 err2
  | TTuple ts1, TTuple ts2 ->
      List.length ts1 = List.length ts2 && List.for_all2 types_equal ts1 ts2
  | TArray (t1, s1), TArray (t2, s2) -> s1 = s2 && types_equal t1 t2
  | TFunc (params1, ret1), TFunc (params2, ret2) ->
      List.length params1 = List.length params2
      && List.for_all2 types_equal params1 params2
      && types_equal ret1 ret2
  | TStruct fields1, TStruct fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all2
           (fun f1 f2 ->
             f1.field_name = f2.field_name
             && types_equal f1.field_type f2.field_type)
           fields1 fields2
  | TUnion (name1, _), TUnion (name2, _) -> name1 = name2
  | _ -> false

(** Token conversion utilities.

    This section provides functions for converting tokens to AST nodes. These
    functions are used by the parser to eliminate code duplication in
    token-to-AST conversion logic. *)

(** Convert token to expression.

    @param token The token to convert
    @return The corresponding expression
    @raise Ast_util_error if the token cannot be converted to expression *)
let token_to_expr token =
  match token with
  | Lexer.I8 n -> EI8 n
  | Lexer.U8 n -> EU8 n
  | Lexer.I16 n -> EI16 n
  | Lexer.U16 n -> EU16 n
  | Lexer.I32 n -> EI32 n
  | Lexer.U32 n -> EU32 n
  | Lexer.I64 n -> EI64 n
  | Lexer.U64 n -> EU64 n
  | Lexer.INT n -> EInt n
  | Lexer.UINT n -> EUint n
  | Lexer.F32 f -> EF32 f
  | Lexer.F64 f -> EF64 f
  | Lexer.FLOAT f -> EFloat f
  | Lexer.BOOL b -> EBool b
  | Lexer.CHAR c -> EChar c
  | Lexer.STRING s -> EString s
  | Lexer.BYTES b -> EBytes b
  | Lexer.UNIT -> EUnit
  | _ -> raise (Ast_util_error "Token cannot be converted to expression")

(** Convert token to type.

    @param token The token to convert
    @return The corresponding type
    @raise Ast_util_error if the token cannot be converted to type *)
let token_to_type token =
  match token with
  | Lexer.KW_I8 -> TI8
  | Lexer.KW_U8 -> TU8
  | Lexer.KW_I16 -> TI16
  | Lexer.KW_U16 -> TU16
  | Lexer.KW_I32 -> TI32
  | Lexer.KW_U32 -> TU32
  | Lexer.KW_I64 -> TI64
  | Lexer.KW_U64 -> TU64
  | Lexer.KW_INT -> TInt
  | Lexer.KW_UINT -> TUint
  | Lexer.KW_F32 -> TF32
  | Lexer.KW_F64 -> TF64
  | Lexer.KW_FLOAT -> TFloat
  | Lexer.KW_BOOL -> TBool
  | Lexer.KW_CHAR -> TChar
  | Lexer.KW_STRING -> TString
  | Lexer.KW_BYTES -> TBytes
  | Lexer.KW_UNIT -> TUnit
  | _ -> raise (Ast_util_error "Token cannot be converted to type")

(** Convert token to binary operator.

    @param token The token to convert
    @return The corresponding binary operator
    @raise Ast_util_error if the token cannot be converted to binary operator *)
let token_to_binop token =
  match token with
  | Lexer.PLUS -> Add
  | Lexer.MINUS -> Sub
  | Lexer.MULT -> Mul
  | Lexer.DIV -> Div
  | Lexer.MOD -> Mod
  | Lexer.EQ -> Eq
  | Lexer.NEQ -> Neq
  | Lexer.LT -> Lt
  | Lexer.LE -> Le
  | Lexer.GT -> Gt
  | Lexer.GE -> Ge
  | Lexer.AND -> And
  | Lexer.OR -> Or
  | Lexer.ASSIGN -> Assign
  | Lexer.PIPE -> Pipe
  | _ -> raise (Ast_util_error "Token cannot be converted to binary operator")

(** Convert token to unary operator.

    @param token The token to convert
    @return The corresponding unary operator
    @raise Ast_util_error if the token cannot be converted to unary operator *)
let token_to_unop token =
  match token with
  | Lexer.NOT -> Not
  | Lexer.MINUS -> Neg
  | _ -> raise (Ast_util_error "Token cannot be converted to unary operator")

(** Get string representation of token for error messages.

    @param token The token to convert to string
    @return String representation of the token *)
let token_to_string token =
  match token with
  | Lexer.KW_FUN -> "KW_FUN"
  | Lexer.KW_FN -> "KW_FN"
  | Lexer.KW_LET -> "KW_LET"
  | Lexer.KW_EXTERN -> "KW_EXTERN"
  | Lexer.KW_UNSAFE -> "KW_UNSAFE"
  | Lexer.KW_IF -> "KW_IF"
  | Lexer.KW_ELSE -> "KW_ELSE"
  | Lexer.KW_MATCH -> "KW_MATCH"
  | Lexer.KW_WITH -> "KW_WITH"
  | Lexer.KW_WHILE -> "KW_WHILE"
  | Lexer.KW_FOR -> "KW_FOR"
  | Lexer.KW_IN -> "KW_IN"
  | Lexer.KW_DO -> "KW_DO"
  | Lexer.KW_SOME -> "KW_SOME"
  | Lexer.KW_NONE -> "KW_NONE"
  | Lexer.KW_OK -> "KW_OK"
  | Lexer.KW_ERR -> "KW_ERR"
  | Lexer.KW_I8 -> "KW_I8"
  | Lexer.KW_U8 -> "KW_U8"
  | Lexer.KW_I16 -> "KW_I16"
  | Lexer.KW_U16 -> "KW_U16"
  | Lexer.KW_I32 -> "KW_I32"
  | Lexer.KW_U32 -> "KW_U32"
  | Lexer.KW_I64 -> "KW_I64"
  | Lexer.KW_U64 -> "KW_U64"
  | Lexer.KW_INT -> "KW_INT"
  | Lexer.KW_UINT -> "KW_UINT"
  | Lexer.KW_F32 -> "KW_F32"
  | Lexer.KW_F64 -> "KW_F64"
  | Lexer.KW_FLOAT -> "KW_FLOAT"
  | Lexer.KW_BOOL -> "KW_BOOL"
  | Lexer.KW_CHAR -> "KW_CHAR"
  | Lexer.KW_STRING -> "KW_STRING"
  | Lexer.KW_BYTES -> "KW_BYTES"
  | Lexer.KW_UNIT -> "KW_UNIT"
  | Lexer.KW_STRUCT -> "KW_STRUCT"
  | Lexer.KW_PRINT -> "KW_PRINT"
  | Lexer.KW_PRINTLN -> "KW_PRINTLN"
  | Lexer.KW_PACKAGE -> "KW_PACKAGE"
  | Lexer.KW_THEN -> "KW_THEN"
  | Lexer.KW_TYPE -> "KW_TYPE"
  | Lexer.KW_UNION -> "KW_UNION"
  | Lexer.KW_IMPORT -> "KW_IMPORT"
  | Lexer.KW_MUTABLE -> "KW_MUTABLE"
  | Lexer.KW_TRUE -> "KW_TRUE"
  | Lexer.KW_FALSE -> "KW_FALSE"
  | Lexer.EQUAL -> "EQUAL"
  | Lexer.PIPE_SYMBOL -> "PIPE_SYMBOL"
  | Lexer.UNDERSCORE -> "UNDERSCORE"
  | Lexer.IDENT s -> sprintf "IDENT(%s)" s
  | Lexer.I8 n -> sprintf "I8(%d)" n
  | Lexer.U8 n -> sprintf "U8(%d)" n
  | Lexer.I16 n -> sprintf "I16(%d)" n
  | Lexer.U16 n -> sprintf "U16(%d)" n
  | Lexer.I32 n -> sprintf "I32(%d)" n
  | Lexer.U32 n -> sprintf "U32(%d)" n
  | Lexer.I64 n -> sprintf "I64(%Ld)" n
  | Lexer.U64 n -> sprintf "U64(%Ld)" n
  | Lexer.INT n -> sprintf "INT(%Ld)" n
  | Lexer.UINT n -> sprintf "UINT(%Ld)" n
  | Lexer.F32 f -> sprintf "F32(%g)" f
  | Lexer.F64 f -> sprintf "F64(%g)" f
  | Lexer.FLOAT f -> sprintf "FLOAT(%g)" f
  | Lexer.BOOL true -> "BOOL(true)"
  | Lexer.BOOL false -> "BOOL(false)"
  | Lexer.CHAR c -> sprintf "CHAR('%c')" c
  | Lexer.STRING s -> sprintf "STRING(\"%s\")" s
  | Lexer.BYTES _ -> "BYTES"
  | Lexer.UNIT -> "UNIT"
  | Lexer.PLUS -> "PLUS"
  | Lexer.MINUS -> "MINUS"
  | Lexer.MULT -> "MULT"
  | Lexer.DIV -> "DIV"
  | Lexer.MOD -> "MOD"
  | Lexer.EQ -> "EQ"
  | Lexer.NEQ -> "NEQ"
  | Lexer.LT -> "LT"
  | Lexer.LE -> "LE"
  | Lexer.GT -> "GT"
  | Lexer.GE -> "GE"
  | Lexer.AND -> "AND"
  | Lexer.OR -> "OR"
  | Lexer.NOT -> "NOT"
  | Lexer.ASSIGN -> "ASSIGN"
  | Lexer.PIPE -> "PIPE"
  | Lexer.LPAREN -> "LPAREN"
  | Lexer.RPAREN -> "RPAREN"
  | Lexer.LBRACE -> "LBRACE"
  | Lexer.RBRACE -> "RBRACE"
  | Lexer.LBRACKET -> "LBRACKET"
  | Lexer.RBRACKET -> "RBRACKET"
  | Lexer.COMMA -> "COMMA"
  | Lexer.SEMICOLON -> "SEMICOLON"
  | Lexer.COLON -> "COLON"
  | Lexer.DOT -> "DOT"
  | Lexer.ARROW -> "ARROW"
  | Lexer.DOUBLE_COLON -> "DOUBLE_COLON"
  | Lexer.EOF -> "EOF"
