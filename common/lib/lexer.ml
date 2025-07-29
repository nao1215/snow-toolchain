(*** @project snow-toolchain @module Lexer

  Lexical analyzer (tokenizer) for the SNOW programming language.

  This module performs the first phase of compilation: converting raw source
  text into a structured stream of tokens that represent lexical elements
  (keywords, identifiers, literals, operators, etc.).

  Key Features: - Complete SNOW syntax support including all numeric types -
  Unicode string and character literals with escape sequences - Single-line (//)
  and multi-line (/* */) comment handling - Type-suffixed numeric literals
  (42i32, 3.14f32) - Comprehensive error reporting with position information

  Design Principles: - Shared tokens: Common token definitions for all compiler
  phases - No backtracking: Single-pass, deterministic tokenization - Error
  recovery: Meaningful error messages with source positions - Extensibility:
  Easy to add new token types and syntax

  Architecture: - Token type defines all possible lexical elements - Utility
  functions for character classification and validation - Keyword table for
  reserved word recognition - Specialized parsers for complex literals (strings,
  numbers) - Main lexer with precedence-based operator recognition

  Used by: - Parser: Consumes token stream to build AST - IDE tools: Syntax
  highlighting, error squiggles - REPL: Interactive tokenization and evaluation
  - Formatters: Token-aware code formatting *)

exception Lex_error of string

type lexer_position = {
  filename : string;
  line : int;
  column : int;
  offset : int;
}
(** Position tracking for better error reporting *)

(** Update position based on character *)
let update_position pos ch =
  if ch = '\n' then
    { pos with line = pos.line + 1; column = 1; offset = pos.offset + 1 }
  else { pos with column = pos.column + 1; offset = pos.offset + 1 }

(** Token type representing all lexical elements in SNOW source code.

    This type defines every possible token that can appear in a SNOW program.
    Tokens are the atomic units produced by lexical analysis and consumed by the
    parser to build the AST.

    Organization:
    - Literals: Direct values (numbers, strings, characters)
    - Keywords: Reserved words with special meaning
    - Operators: Symbols for computations and comparisons
    - Delimiters: Punctuation for structure (parentheses, brackets)
    - Special: EOF and other meta-tokens

    Design notes:
    - Literal tokens carry their values for immediate use
    - Keyword tokens distinguished from identifiers for parsing
    - Operator precedence handled during parsing, not tokenization
    - Position information tracked separately by parser *)
type token =
  (* === Literal tokens (carry actual values) === *)
  | I8 of int  (** 8-bit signed integer: 42i8 *)
  | U8 of int  (** 8-bit unsigned integer: 255u8 *)
  | I16 of int  (** 16-bit signed integer: 1000i16 *)
  | U16 of int  (** 16-bit unsigned integer: 65535u16 *)
  | I32 of int  (** 32-bit signed integer: 42i32 *)
  | U32 of int  (** 32-bit unsigned integer: 42u32 *)
  | I64 of int64  (** 64-bit signed integer: 42i64 *)
  | U64 of int64  (** 64-bit unsigned integer: 42u64 *)
  | INT of int64  (** Default integer literal: 42 (inferred as i64) *)
  | UINT of int64  (** Explicit unsigned integer: 42uint *)
  | F32 of float  (** 32-bit IEEE 754 float: 3.14f32 *)
  | F64 of float  (** 64-bit IEEE 754 float: 3.14f64 *)
  | FLOAT of float  (** Default float literal: 3.14 (inferred as f64) *)
  | BOOL of bool  (** Boolean literal: true or false *)
  | CHAR of char  (** Unicode character: 'A', '\n', '\u{1F600}' *)
  | STRING of string  (** UTF-8 string: "hello", "world\n" *)
  | BYTES of bytes  (** Raw byte array: [0x48, 0x65, 0x6C, 0x6C, 0x6F] *)
  | UNIT  (** Unit value: () *)
  (* === Identifiers and user-defined names === *)
  | IDENT of string  (** User identifier: variable, function names *)
  (* === Type keywords for type annotations === *)
  | KW_I8
  | KW_U8
  | KW_I16
  | KW_U16
  | KW_I32
  | KW_U32
  | KW_I64
  | KW_U64  (** Fixed-width integer type keywords *)
  | KW_INT
  | KW_UINT
  | KW_F32
  | KW_F64
  | KW_FLOAT
  | KW_BOOL
  | KW_CHAR  (** Default and floating-point type keywords *)
  | KW_STRING
  | KW_BYTES
  | KW_UNIT  (** String, binary data, and unit type keywords *)
  (* === Control flow keywords === *)
  | KW_IF
  | KW_THEN
  | KW_ELSE  (** Conditional expressions: if cond then expr else expr *)
  | KW_MATCH
  | KW_WITH  (** Pattern matching: match expr with | pattern -> expr *)
  | KW_WHILE
  | KW_FOR
  | KW_DO
  (* === Function and binding keywords === *)
  | KW_LET
  | KW_FUN
  | KW_FN  (** Alternative function keyword for extern declarations *)
  | KW_IN  (** Function definition, let bindings, and for loops *)
  | KW_EXTERN  (** External function declarations for FFI *)
  (* === Type definition keywords === *)
  | KW_TYPE
  | KW_STRUCT
  | KW_UNION  (** User-defined types: algebraic data types and structures *)
  (* === Module system keywords === *)
  | KW_PACKAGE
  | KW_IMPORT  (** Package declarations and imports *)
  (* === Option/Result constructors === *)
  | KW_SOME
  | KW_NONE
  | KW_OK
  | KW_ERR  (** Built-in variant constructors for Option and Result types *)
  (* === Built-in functions === *)
  | KW_PRINT
  | KW_PRINTLN  (** Built-in output functions: print(), println() *)
  (* === Other language keywords === *)
  | KW_MUTABLE
  | KW_TRUE
  | KW_FALSE  (** Mutability modifier and boolean literals *)
  | KW_UNSAFE  (** Unsafe code marker *)
  (* === Operators (precedence handled in parser) === *)
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD  (** Arithmetic operators: +, -, *, /, % *)
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE  (** Comparison operators: ==, !=, <, <=, >, >= *)
  | AND
  | OR
  | NOT  (** Logical operators: &&, ||, ! (short-circuiting) *)
  | ASSIGN  (** Assignment operator: := (for mutable variables) *)
  | PIPE  (** Pipe operator: |> (function composition, left-to-right) *)
  | ARROW  (** Arrow operator: -> (function types, case arms) *)
  | DOUBLE_COLON  (** Double colon: :: (type ascription, module access) *)
  (* === Delimiters and punctuation === *)
  | LPAREN
  | RPAREN  (** Parentheses: (, ) (grouping, tuples, function calls) *)
  | LBRACE
  | RBRACE  (** Braces: {, } (struct literals, blocks) *)
  | LBRACKET
  | RBRACKET  (** Brackets: [, ] (arrays, slices, indexing) *)
  | COMMA
  | SEMICOLON
  | COLON
      (** Punctuation: , (separators), ; (terminators), : (type annotations) *)
  | DOT
  | EQUAL  (** Access and binding: . (field access), = (let bindings) *)
  | PIPE_SYMBOL  (** Pipe symbol: | (pattern matching, algebraic data types) *)
  | UNDERSCORE  (** Underscore: _ (wildcard patterns, discarded values) *)
  (* === Special meta-tokens === *)
  | EOF  (** End of file marker (signals completion of input stream) *)

(** Character classification functions for lexical analysis. *)

(** Check if character is a decimal digit (0-9). Used for parsing integer and
    floating-point literals. *)
let is_digit c = '0' <= c && c <= '9'

(** Check if character can start an identifier (letter or underscore). SNOW
    identifiers follow Unicode identifier standards. *)
let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

(** Check if character can continue an identifier (alphanumeric or underscore).
    Used after the first character of an identifier. *)
let is_alnum c = is_alpha c || is_digit c

(** Check if character is a hexadecimal digit (0-9, a-f, A-F). Used for parsing
    hexadecimal integer literals and Unicode escapes. *)
let is_hex_digit c =
  is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')

(** Check if character is a binary digit (0 or 1). Used for parsing binary
    integer literals (0b prefix). *)
let is_bin_digit c = c = '0' || c = '1'

(** Check if character is an octal digit (0-7). Used for parsing octal integer
    literals (0o prefix). *)
let is_oct_digit c = '0' <= c && c <= '7'

(** List of reserved keywords in SNOW language. *)
let keywords : (string * token) list =
  [
    (* Type keywords *)
    ("i8", KW_I8);
    ("u8", KW_U8);
    ("i16", KW_I16);
    ("u16", KW_U16);
    ("i32", KW_I32);
    ("u32", KW_U32);
    ("i64", KW_I64);
    ("u64", KW_U64);
    ("int", KW_INT);
    ("uint", KW_UINT);
    ("f32", KW_F32);
    ("f64", KW_F64);
    ("float", KW_FLOAT);
    ("bool", KW_BOOL);
    ("char", KW_CHAR);
    ("string", KW_STRING);
    ("bytes", KW_BYTES);
    ("unit", KW_UNIT);
    (* Control flow keywords *)
    ("if", KW_IF);
    ("then", KW_THEN);
    ("else", KW_ELSE);
    ("match", KW_MATCH);
    ("with", KW_WITH);
    ("while", KW_WHILE);
    ("for", KW_FOR);
    ("do", KW_DO);
    (* Function keywords *)
    ("let", KW_LET);
    ("fun", KW_FUN);
    ("fn", KW_FN);
    ("in", KW_IN);
    ("extern", KW_EXTERN);
    (* Type definition keywords *)
    ("type", KW_TYPE);
    ("struct", KW_STRUCT);
    ("union", KW_UNION);
    (* Module keywords *)
    ("package", KW_PACKAGE);
    ("import", KW_IMPORT);
    (* Option/Result keywords *)
    ("Some", KW_SOME);
    ("None", KW_NONE);
    ("Ok", KW_OK);
    ("Err", KW_ERR);
    (* Built-in functions *)
    ("print", KW_PRINT);
    ("println", KW_PRINTLN);
    (* Other keywords *)
    ("mutable", KW_MUTABLE);
    ("true", KW_TRUE);
    ("false", KW_FALSE);
    ("unsafe", KW_UNSAFE);
  ]

(** Skip whitespace and comments, returning the next significant character
    position.

    Handles both single-line comments (//) and multi-line comments (/* ... */).
    Recursively processes nested comments and whitespace until a meaningful
    character is found or end of input is reached.

    Parameters:
    - input: Source code string to process
    - len: Length of input string (for bounds checking)
    - i: Current position in input string

    Returns: Position of next non-whitespace, non-comment character *)
let rec skip input len i =
  if i >= len then i
  else
    match (input.[i], i + 1 < len) with
    | '/', true when String.sub input i 2 = "//" ->
        (* single-line comment *)
        let j =
          try String.index_from input (i + 2) '\n' with Not_found -> len
        in
        skip input len (j + 1)
    | '/', true when String.sub input i 2 = "/*" ->
        (* multi-line comment *)
        let rec find_end k =
          if k + 1 >= len then raise (Lex_error "Unterminated comment")
          else if input.[k] = '*' && input.[k + 1] = '/' then k + 2
          else find_end (k + 1)
        in
        skip input len (find_end (i + 2))
    | c, _ when c = ' ' || c = '\n' || c = '\t' || c = '\r' ->
        skip input len (i + 1)
    | _ -> i

(** Parse a character literal with escape sequence support.

    Handles both regular characters ('A') and escape sequences ('\n', '\t',
    etc.). Supports standard C-style escape sequences including null terminator.

    Parameters:
    - input: Source code string
    - i: Starting position (should point to opening quote)
    - len: Input string length

    Returns: (parsed_character, next_position) tuple Raises: Lex_error for
    invalid escape sequences or unterminated literals *)
let parse_char input i len =
  if i + 2 >= len then raise (Lex_error "Unterminated character literal")
  else if input.[i + 1] = '\\' then
    (* Escape sequence *)
    if i + 3 >= len then raise (Lex_error "Unterminated character literal")
    else
      let escaped_char =
        match input.[i + 2] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '\"' -> '\"'
        | '0' -> '\000'
        | c ->
            raise
              (Lex_error (Printf.sprintf "Invalid escape sequence '\\%c'" c))
      in
      if input.[i + 3] = '\'' then (escaped_char, i + 4)
      else raise (Lex_error "Expected closing quote for character literal")
  else if
    (* Regular character *)
    input.[i + 2] = '\''
  then (input.[i + 1], i + 3)
  else raise (Lex_error "Expected closing quote for character literal")

(** Parse a string literal with escape sequence support.

    Processes UTF-8 string literals enclosed in double quotes. Supports standard
    escape sequences and accumulates characters into a buffer for efficient
    string construction.

    Parameters:
    - input: Source code string
    - i: Starting position (should point to opening quote)
    - len: Input string length

    Returns: (parsed_string, next_position) tuple Raises: Lex_error for invalid
    escape sequences or unterminated strings *)
let parse_string input i len =
  let buf = Buffer.create 16 in
  let rec loop j =
    if j >= len then raise (Lex_error "Unterminated string literal")
    else
      match input.[j] with
      | '\"' -> (Buffer.contents buf, j + 1)
      | '\\' when j + 1 < len ->
          let escaped_char =
            match input.[j + 1] with
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | '\\' -> '\\'
            | '\'' -> '\''
            | '\"' -> '\"'
            | '0' -> '\000'
            | c ->
                raise
                  (Lex_error (Printf.sprintf "Invalid escape sequence '\\%c'" c))
          in
          Buffer.add_char buf escaped_char;
          loop (j + 2)
      | c ->
          Buffer.add_char buf c;
          loop (j + 1)
  in
  loop (i + 1)

(** Parse byte array literal: [0x12, 0x34, 0xAB, ...]

    Handles hexadecimal byte literals enclosed in square brackets. Each byte
    must be a valid 2-digit hex value (0x00 to 0xFF).

    Parameters:
    - input: Source code string
    - i: Starting position (should point to opening bracket)
    - len: Input string length

    Returns: (BYTES_token, next_position) tuple Raises: Lex_error for invalid
    hex values or unterminated literals *)
let parse_bytes input i len =
  let bytes_list = ref [] in
  let j = ref (i + 1) in
  (* Skip opening '[' *)

  let rec parse_byte_list () =
    j := skip input len !j;
    if !j >= len then raise (Lex_error "Unterminated byte array literal")
    else if input.[!j] = ']' then incr j (* Skip closing ']' *)
    else if input.[!j] = '0' && !j + 1 < len && input.[!j + 1] = 'x' then
      (* Parse 0x## format *)
      if !j + 3 >= len then
        raise
          (Lex_error "Invalid hex byte literal: expected 2 hex digits after 0x")
      else
        let hex_str = String.sub input (!j + 2) 2 in
        try
          let byte_val = int_of_string ("0x" ^ hex_str) in
          if byte_val < 0 || byte_val > 255 then
            raise (Lex_error "Byte value out of range (0x00-0xFF)")
          else bytes_list := byte_val :: !bytes_list;
          j := !j + 4;
          (* Skip "0x##" *)
          j := skip input len !j;
          if !j < len && input.[!j] = ',' then (
            incr j;
            (* Skip comma *)
            parse_byte_list ())
          else if !j < len && input.[!j] = ']' then
            incr j (* Skip closing ']' *)
          else raise (Lex_error "Expected ',' or ']' in byte array")
        with Failure _ ->
          raise (Lex_error ("Invalid hex digit in byte literal: " ^ hex_str))
    else raise (Lex_error "Expected hex byte literal (0x##) or ']'")
  in

  parse_byte_list ();
  let bytes_array = Array.of_list (List.rev !bytes_list) in
  let bytes_string = Bytes.create (Array.length bytes_array) in
  Array.iteri
    (fun idx byte -> Bytes.set_uint8 bytes_string idx byte)
    bytes_array;
  (BYTES bytes_string, !j)

(** Parse numeric literals with optional type suffixes.

    Handles both integer and floating-point literals with explicit type
    annotations. Supports type suffixes like i32, u64, f32, f64, uint, etc.
    Automatically determines whether the literal is integer or float based on
    decimal point presence.

    Examples:
    - 42 -> INT(42)
    - 42i32 -> I32(42)
    - 3.14 -> FLOAT(3.14)
    - 2.5f32 -> F32(2.5)

    Parameters:
    - input: Source code string
    - i: Starting position of the number
    - len: Input string length

    Returns: (token, next_position) tuple where token contains parsed value
    Raises: Lex_error for invalid suffixes or malformed numbers *)
let parse_number input i len =
  let j = ref i in
  let has_dot = ref false in

  (* Parse digits and optional decimal point *)
  while
    !j < len && (is_digit input.[!j] || (input.[!j] = '.' && not !has_dot))
  do
    if input.[!j] = '.' then has_dot := true;
    incr j
  done;

  let number_str = String.sub input i (!j - i) in

  (* Check for type suffix *)
  let suffix_start = !j in
  while !j < len && is_alnum input.[!j] do
    incr j
  done;

  let suffix =
    if !j > suffix_start then
      Some (String.sub input suffix_start (!j - suffix_start))
    else None
  in

  let token =
    match (!has_dot, suffix) with
    | true, None -> FLOAT (float_of_string number_str)
    | true, Some "f32" -> F32 (float_of_string number_str)
    | true, Some "f64" -> F64 (float_of_string number_str)
    | true, Some s ->
        raise (Lex_error (Printf.sprintf "Invalid float suffix '%s'" s))
    | false, None -> INT (Int64.of_string number_str)
    | false, Some "i8" -> I8 (int_of_string number_str)
    | false, Some "u8" -> U8 (int_of_string number_str)
    | false, Some "i16" -> I16 (int_of_string number_str)
    | false, Some "u16" -> U16 (int_of_string number_str)
    | false, Some "i32" -> I32 (int_of_string number_str)
    | false, Some "u32" -> U32 (int_of_string number_str)
    | false, Some "i64" -> I64 (Int64.of_string number_str)
    | false, Some "u64" -> U64 (Int64.of_string number_str)
    | false, Some "uint" -> UINT (Int64.of_string number_str)
    | false, Some s ->
        raise (Lex_error (Printf.sprintf "Invalid integer suffix '%s'" s))
  in
  (token, !j)

(** Main lexical analysis function - converts source code to token stream.

    Performs complete tokenization of SNOW source code in a single pass. Uses
    recursive descent approach with specialized parsers for different token
    types. Handles operator precedence, comment skipping, and error recovery.

    Process: 1. Skip whitespace and comments 2. Identify token type by first
    character(s) 3. Dispatch to appropriate specialized parser 4. Accumulate
    tokens in reverse order for efficiency 5. Return complete token list with
    EOF marker

    Parameters:
    - input: Complete source code string to tokenize

    Returns: List of tokens in source order, terminated with EOF Raises:
    Lex_error for invalid characters or malformed tokens *)
let lex input =
  let len = String.length input in

  let rec next i acc =
    let i = skip input len i in
    if i >= len then List.rev (EOF :: acc)
    else
      match input.[i] with
      (* Multi-character operators *)
      | ':' when i + 1 < len && input.[i + 1] = '=' ->
          next (i + 2) (ASSIGN :: acc)
      | ':' when i + 1 < len && input.[i + 1] = ':' ->
          next (i + 2) (DOUBLE_COLON :: acc)
      | '|' when i + 1 < len && input.[i + 1] = '>' -> next (i + 2) (PIPE :: acc)
      | '-' when i + 1 < len && input.[i + 1] = '>' ->
          next (i + 2) (ARROW :: acc)
      | '&' when i + 1 < len && input.[i + 1] = '&' -> next (i + 2) (AND :: acc)
      | '|' when i + 1 < len && input.[i + 1] = '|' -> next (i + 2) (OR :: acc)
      | '=' when i + 1 < len && input.[i + 1] = '=' -> next (i + 2) (EQ :: acc)
      | '!' when i + 1 < len && input.[i + 1] = '=' -> next (i + 2) (NEQ :: acc)
      | '<' when i + 1 < len && input.[i + 1] = '=' -> next (i + 2) (LE :: acc)
      | '>' when i + 1 < len && input.[i + 1] = '=' -> next (i + 2) (GE :: acc)
      (* Single-character operators and delimiters *)
      | '+' -> next (i + 1) (PLUS :: acc)
      | '-' -> next (i + 1) (MINUS :: acc)
      | '*' -> next (i + 1) (MULT :: acc)
      | '/' -> next (i + 1) (DIV :: acc)
      | '%' -> next (i + 1) (MOD :: acc)
      | '<' -> next (i + 1) (LT :: acc)
      | '>' -> next (i + 1) (GT :: acc)
      | '!' -> next (i + 1) (NOT :: acc)
      | '(' when i + 1 < len && input.[i + 1] = ')' -> next (i + 2) (UNIT :: acc)
      | '(' -> next (i + 1) (LPAREN :: acc)
      | ')' -> next (i + 1) (RPAREN :: acc)
      | '{' -> next (i + 1) (LBRACE :: acc)
      | '}' -> next (i + 1) (RBRACE :: acc)
      | '[' when i + 2 < len && input.[i + 1] = '0' && input.[i + 2] = 'x' ->
          (* Byte array literal: [0x##, 0x##, ...] *)
          let token, new_i = parse_bytes input i len in
          next new_i (token :: acc)
      | '[' -> next (i + 1) (LBRACKET :: acc)
      | ']' -> next (i + 1) (RBRACKET :: acc)
      | ',' -> next (i + 1) (COMMA :: acc)
      | ';' -> next (i + 1) (SEMICOLON :: acc)
      | ':' -> next (i + 1) (COLON :: acc)
      | '.' -> next (i + 1) (DOT :: acc)
      | '=' -> next (i + 1) (EQUAL :: acc)
      | '|' -> next (i + 1) (PIPE_SYMBOL :: acc)
      (* Character literal *)
      | '\'' ->
          let char_val, new_i = parse_char input i len in
          next new_i (CHAR char_val :: acc)
      (* String literal *)
      | '\"' ->
          let string_val, new_i = parse_string input i len in
          next new_i (STRING string_val :: acc)
      (* Numbers *)
      | c when is_digit c ->
          let token, new_i = parse_number input i len in
          next new_i (token :: acc)
      (* Identifiers and keywords (including underscore-prefixed) *)
      | c when is_alpha c ->
          let j = ref i in
          while !j < len && is_alnum input.[!j] do
            incr j
          done;
          let s = String.sub input i (!j - i) in
          let token =
            match List.assoc_opt s keywords with
            | Some kw -> kw
            | None -> IDENT s
          in
          next !j (token :: acc)
      (* Standalone underscore *)
      | '_' -> next (i + 1) (UNDERSCORE :: acc)
      | c ->
          raise
            (Lex_error
               (Printf.sprintf "Unexpected character '%c' at position %d" c i))
  in
  next 0 []
