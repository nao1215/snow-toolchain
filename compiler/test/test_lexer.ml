open Alcotest
open Lexer

(** Convert a token to its string representation for checking. *)
let string_of_tok = function
  (* Literals *)
  | I8 n -> Printf.sprintf "I8(%d)" n
  | U8 n -> Printf.sprintf "U8(%d)" n
  | I16 n -> Printf.sprintf "I16(%d)" n
  | U16 n -> Printf.sprintf "U16(%d)" n
  | I32 n -> Printf.sprintf "I32(%d)" n
  | U32 n -> Printf.sprintf "U32(%d)" n
  | I64 n -> Printf.sprintf "I64(%Ld)" n
  | U64 n -> Printf.sprintf "U64(%Ld)" n
  | INT n -> Printf.sprintf "INT(%Ld)" n
  | UINT n -> Printf.sprintf "UINT(%Ld)" n
  | F32 f -> Printf.sprintf "F32(%g)" f
  | F64 f -> Printf.sprintf "F64(%g)" f
  | FLOAT f -> Printf.sprintf "FLOAT(%g)" f
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | CHAR c -> Printf.sprintf "CHAR('%c')" c
  | STRING s -> Printf.sprintf "STRING(\"%s\")" s
  | BYTES _ -> "BYTES(<bytes>)"
  | UNIT -> "UNIT"
  (* Identifiers and Keywords *)
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  (* Type keywords *)
  | KW_I8 -> "KW_I8"
  | KW_U8 -> "KW_U8"
  | KW_I16 -> "KW_I16"
  | KW_U16 -> "KW_U16"
  | KW_I32 -> "KW_I32"
  | KW_U32 -> "KW_U32"
  | KW_I64 -> "KW_I64"
  | KW_U64 -> "KW_U64"
  | KW_INT -> "KW_INT"
  | KW_UINT -> "KW_UINT"
  | KW_F32 -> "KW_F32"
  | KW_F64 -> "KW_F64"
  | KW_FLOAT -> "KW_FLOAT"
  | KW_BOOL -> "KW_BOOL"
  | KW_CHAR -> "KW_CHAR"
  | KW_STRING -> "KW_STRING"
  | KW_BYTES -> "KW_BYTES"
  | KW_UNIT -> "KW_UNIT"
  (* Control flow keywords *)
  | KW_IF -> "KW_IF"
  | KW_THEN -> "KW_THEN"
  | KW_ELSE -> "KW_ELSE"
  | KW_MATCH -> "KW_MATCH"
  | KW_WITH -> "KW_WITH"
  | KW_WHILE -> "KW_WHILE"
  | KW_FOR -> "KW_FOR"
  | KW_DO -> "KW_DO"
  (* Function keywords *)
  | KW_LET -> "KW_LET"
  | KW_FUN -> "KW_FUN"
  | KW_IN -> "KW_IN"
  (* Type definition keywords *)
  | KW_TYPE -> "KW_TYPE"
  | KW_STRUCT -> "KW_STRUCT"
  | KW_UNION -> "KW_UNION"
  (* Module keywords *)
  | KW_PACKAGE -> "KW_PACKAGE"
  | KW_IMPORT -> "KW_IMPORT"
  (* Option/Result keywords *)
  | KW_SOME -> "KW_SOME"
  | KW_NONE -> "KW_NONE"
  | KW_OK -> "KW_OK"
  | KW_ERR -> "KW_ERR"
  (* Other keywords *)
  | KW_MUTABLE -> "KW_MUTABLE"
  | KW_TRUE -> "KW_TRUE"
  | KW_FALSE -> "KW_FALSE"
  (* Operators *)
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | ASSIGN -> "ASSIGN"
  | PIPE -> "PIPE"
  | ARROW -> "ARROW"
  | DOUBLE_COLON -> "DOUBLE_COLON"
  (* Delimiters *)
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | DOT -> "DOT"
  | EQUAL -> "EQUAL"
  | PIPE_SYMBOL -> "PIPE_SYMBOL"
  | UNDERSCORE -> "UNDERSCORE"
  (* Built-in functions *)
  | KW_PRINT -> "KW_PRINT"
  | KW_PRINTLN -> "KW_PRINTLN"
  | KW_EXTERN -> "KW_EXTERN"
  | KW_FN -> "KW_FN"
  | KW_UNSAFE -> "KW_UNSAFE"
  (* Special *)
  | EOF -> "EOF"

let make_case (name, input, expected) =
  test_case name `Quick (fun () ->
      let toks = lex input |> List.map string_of_tok in
      check (list string) name expected toks)

let test_cases =
  [
    (* Basic numeric literals *)
    ("integer", "123", [ "INT(123)"; "EOF" ]);
    ("typed integers", "42i32 255u8", [ "I32(42)"; "U8(255)"; "EOF" ]);
    ("float literals", "3.14 2.71f32", [ "FLOAT(3.14)"; "F32(2.71)"; "EOF" ]);
    (* Character and string literals *)
    ("char literal", "'A'", [ "CHAR('A')"; "EOF" ]);
    ("string literal", "\"hello\"", [ "STRING(\"hello\")"; "EOF" ]);
    ( "escape sequences",
      "'\\n' \"\\\"quoted\\\"\"",
      [ "CHAR('\n')"; "STRING(\"\"quoted\"\")"; "EOF" ] );
    (* Keywords *)
    ("bool keywords", "true false", [ "KW_TRUE"; "KW_FALSE"; "EOF" ]);
    ( "type keywords",
      "i32 bool string",
      [ "KW_I32"; "KW_BOOL"; "KW_STRING"; "EOF" ] );
    ( "control keywords",
      "if then else match",
      [ "KW_IF"; "KW_THEN"; "KW_ELSE"; "KW_MATCH"; "EOF" ] );
    ("function keywords", "let fun in", [ "KW_LET"; "KW_FUN"; "KW_IN"; "EOF" ]);
    (* Identifiers *)
    ( "identifiers",
      "x myVar _private",
      [ "IDENT(x)"; "IDENT(myVar)"; "IDENT(_private)"; "EOF" ] );
    (* Operators *)
    ( "arithmetic ops",
      "+ - * / %",
      [ "PLUS"; "MINUS"; "MULT"; "DIV"; "MOD"; "EOF" ] );
    ( "comparison ops",
      "== != < <= > >=",
      [ "EQ"; "NEQ"; "LT"; "LE"; "GT"; "GE"; "EOF" ] );
    ("logical ops", "&& || !", [ "AND"; "OR"; "NOT"; "EOF" ]);
    ( "special ops",
      ":= |> -> ::",
      [ "ASSIGN"; "PIPE"; "ARROW"; "DOUBLE_COLON"; "EOF" ] );
    (* Delimiters *)
    ( "delimiters",
      "( ) {} [] , ; :",
      [
        "LPAREN";
        "RPAREN";
        "LBRACE";
        "RBRACE";
        "LBRACKET";
        "RBRACKET";
        "COMMA";
        "SEMICOLON";
        "COLON";
        "EOF";
      ] );
    ("more delimiters", ". = |", [ "DOT"; "EQUAL"; "PIPE_SYMBOL"; "EOF" ]);
    (* Unit literal *)
    ("unit literal", "()", [ "UNIT"; "EOF" ]);
    (* Complex expressions *)
    ( "function def",
      "let add = fun(x: i32, y: i32): i32 -> x + y",
      [
        "KW_LET";
        "IDENT(add)";
        "EQUAL";
        "KW_FUN";
        "LPAREN";
        "IDENT(x)";
        "COLON";
        "KW_I32";
        "COMMA";
        "IDENT(y)";
        "COLON";
        "KW_I32";
        "RPAREN";
        "COLON";
        "KW_I32";
        "ARROW";
        "IDENT(x)";
        "PLUS";
        "IDENT(y)";
        "EOF";
      ] );
    ( "match expression",
      "match opt: | Some(x) -> x | None -> 0",
      [
        "KW_MATCH";
        "IDENT(opt)";
        "COLON";
        "PIPE_SYMBOL";
        "KW_SOME";
        "LPAREN";
        "IDENT(x)";
        "RPAREN";
        "ARROW";
        "IDENT(x)";
        "PIPE_SYMBOL";
        "KW_NONE";
        "ARROW";
        "INT(0)";
        "EOF";
      ] );
    (* Comments *)
    ( "single line comment",
      "42 // this is a comment\n43",
      [ "INT(42)"; "INT(43)"; "EOF" ] );
    ("multi line comment", "/* start\ncomment */ 42", [ "INT(42)"; "EOF" ]);
  ]

let () = run "Lexer Tests" [ ("lexing", List.map make_case test_cases) ]
