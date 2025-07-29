(*** @project snow-toolchain @module Error_Reporting

  User-friendly error reporting system for the SNOW compiler.

  This module provides comprehensive error reporting with: - Clear,
  human-readable error messages - Source code context and highlighting - Helpful
  suggestions for common mistakes - Consistent formatting across all compiler
  phases

  Design principles: - User-centric: Written for developers, not compiler
  experts - Contextual: Show the relevant source code - Actionable: Provide
  suggestions when possible - Consistent: Uniform format across lexer, parser,
  type checker

  Error categories: - Syntax errors: Lexical and parsing issues - Type errors:
  Type checking failures - Semantic errors: Invalid program structure - File
  errors: I/O and system issues *)

open Printf

type position = { filename : string; line : int; column : int; offset : int }
(** Source position information for error reporting *)

(** Error severity levels *)
type severity = Error | Warning | Note

(** Error category for better organization *)
type error_category =
  | LexicalError
  | SyntaxError
  | TypeError
  | SemanticError
  | FileError
  | CompilerError

type error_info = {
  category : error_category;
  severity : severity;
  code : string;
  title : string;
  message : string;
  position : position option;
  suggestions : string list;
  source_context : string option;
}
(** Comprehensive error information *)

(** ANSI color codes for terminal output *)
let color_reset = "\027[0m"

let color_red = "\027[31m"
let color_yellow = "\027[33m"
let color_blue = "\027[34m"
let color_bold = "\027[1m"
let color_dim = "\027[2m"

(** Format severity for display *)
let severity_to_string = function
  | Error -> sprintf "%serror%s" color_red color_reset
  | Warning -> sprintf "%swarning%s" color_yellow color_reset
  | Note -> sprintf "%snote%s" color_blue color_reset

(** Format error category *)
let category_to_string = function
  | LexicalError -> "lexical"
  | SyntaxError -> "syntax"
  | TypeError -> "type"
  | SemanticError -> "semantic"
  | FileError -> "file"
  | CompilerError -> "compiler"

(** Extract source line from content *)
let get_source_line content line_num =
  let lines = String.split_on_char '\n' content in
  if line_num > 0 && line_num <= List.length lines then
    Some (List.nth lines (line_num - 1))
  else None

(** Create visual indicator for error position *)
let create_position_indicator column_num length =
  let spaces = String.make (column_num - 1) ' ' in
  let carets = String.make (max 1 length) '^' in
  sprintf "%s%s%s%s" spaces color_red carets color_reset

(** Format source context with highlighting *)
let format_source_context error_info source_content =
  match error_info.position with
  | None -> None
  | Some pos -> (
      match get_source_line source_content pos.line with
      | None -> None
      | Some line ->
          let line_num_str = string_of_int pos.line in
          let padding = String.make (String.length line_num_str) ' ' in
          Some
            (sprintf "%s %s %s\n%s %s %s\n"
               (color_dim ^ line_num_str ^ color_reset)
               (color_blue ^ "|" ^ color_reset)
               line
               (color_dim ^ padding ^ color_reset)
               (color_blue ^ "|" ^ color_reset)
               (create_position_indicator pos.column 1)))

(** Format error message for display *)
let format_error_message error_info source_content =
  let header =
    sprintf "%s[%s%s%s]: %s%s%s"
      (severity_to_string error_info.severity)
      (color_dim ^ error_info.code ^ color_reset)
      (color_dim ^ ":" ^ category_to_string error_info.category ^ color_reset)
      ""
      (color_bold ^ error_info.title ^ color_reset)
      "" color_reset
  in

  let location =
    match error_info.position with
    | None -> ""
    | Some pos ->
        sprintf " %s--> %s:%d:%d%s\n" color_dim pos.filename pos.line pos.column
          color_reset
  in

  let source_ctx =
    match format_source_context error_info source_content with
    | None -> ""
    | Some ctx -> "\n" ^ ctx
  in

  let message_body = sprintf "\n%s\n" error_info.message in

  let suggestions =
    match error_info.suggestions with
    | [] -> ""
    | suggestions ->
        let formatted_suggestions = List.map (sprintf "  - %s") suggestions in
        sprintf "\n%shelp%s: try the following:\n%s\n" color_blue color_reset
          (String.concat "\n" formatted_suggestions)
  in

  sprintf "%s%s%s%s%s" header location source_ctx message_body suggestions

(** Create error for lexical issues *)
let lexical_error ~filename ~line ~column ~offset ~code ~title ~message
    ?(suggestions = []) () =
  {
    category = LexicalError;
    severity = Error;
    code;
    title;
    message;
    position = Some { filename; line; column; offset };
    suggestions;
    source_context = None;
  }

(** Create error for syntax issues *)
let syntax_error ~filename ~line ~column ~offset ~code ~title ~message
    ?(suggestions = []) () =
  {
    category = SyntaxError;
    severity = Error;
    code;
    title;
    message;
    position = Some { filename; line; column; offset };
    suggestions;
    source_context = None;
  }

(** Create error for type issues *)
let type_error ~filename ~line ~column ~offset ~code ~title ~message
    ?(suggestions = []) () =
  {
    category = TypeError;
    severity = Error;
    code;
    title;
    message;
    position = Some { filename; line; column; offset };
    suggestions;
    source_context = None;
  }

(** Create error for file issues *)
let file_error ~filename ~code ~title ~message ?(suggestions = []) () =
  {
    category = FileError;
    severity = Error;
    code;
    title;
    message;
    position = Some { filename; line = 1; column = 1; offset = 0 };
    suggestions;
    source_context = None;
  }

(** Create compiler internal error *)
let compiler_error ~code ~title ~message ?(suggestions = []) () =
  {
    category = CompilerError;
    severity = Error;
    code;
    title;
    message;
    position = None;
    suggestions;
    source_context = None;
  }

(** Print error to stderr *)
let print_error error_info source_content =
  let formatted = format_error_message error_info source_content in
  eprintf "%s\n" formatted;
  flush stderr

(** Common error patterns with predefined messages *)

(** Unterminated string literal *)
let unterminated_string_error ~filename ~line ~column ~offset () =
  lexical_error ~filename ~line ~column ~offset ~code:"E001"
    ~title:"Unterminated string literal"
    ~message:"This string is missing its closing quote."
    ~suggestions:
      [
        "Add a closing double quote (\") at the end of the string";
        "Check for unescaped quotes inside the string";
        "Use escape sequences like \\\" for quotes within strings";
      ]
    ()

(** Invalid escape sequence *)
let invalid_escape_error ~filename ~line ~column ~offset ~escape_char () =
  lexical_error ~filename ~line ~column ~offset ~code:"E002"
    ~title:"Invalid escape sequence"
    ~message:
      (sprintf "The escape sequence '\\%c' is not recognized." escape_char)
    ~suggestions:
      [
        "Valid escape sequences: \\n, \\t, \\r, \\\\, \\\", \\0";
        sprintf "Use \\\\%c if you want a literal backslash followed by '%c'"
          escape_char escape_char;
        "Check the SNOW language documentation for supported escape sequences";
      ]
    ()

(** Expected token *)
let expected_token_error ~filename ~line ~column ~offset ~expected ~found () =
  syntax_error ~filename ~line ~column ~offset ~code:"E003"
    ~title:"Unexpected token"
    ~message:(sprintf "Expected '%s' but found '%s'." expected found)
    ~suggestions:
      [
        sprintf "Add the missing '%s' token" expected;
        "Check that all brackets and parentheses are properly matched";
        "Review the syntax for this language construct";
      ]
    ()

(** Missing main function *)
let missing_main_error ~filename () =
  syntax_error ~filename ~line:1 ~column:1 ~offset:0 ~code:"E004"
    ~title:"Missing main function"
    ~message:
      "Every SNOW program must have a 'main' function as the entry point."
    ~suggestions:
      [
        "Add a main function: let main = { /* your code here */ }";
        "Ensure the main function is defined at the top level";
        "Check that 'main' is spelled correctly";
      ]
    ()

(** Type mismatch *)
let type_mismatch_error ~filename ~line ~column ~offset ~expected_type
    ~actual_type () =
  type_error ~filename ~line ~column ~offset ~code:"E005" ~title:"Type mismatch"
    ~message:
      (sprintf "Expected type '%s' but got '%s'." expected_type actual_type)
    ~suggestions:
      [
        sprintf "Convert the value to type '%s'" expected_type;
        "Check that you're using the correct variable or function";
        "Review the type annotations for this expression";
      ]
    ()

(** File not found *)
let file_not_found_error ~filename () =
  file_error ~filename ~code:"E006" ~title:"File not found"
    ~message:(sprintf "Could not find the file '%s'." filename)
    ~suggestions:
      [
        "Check that the file path is correct";
        "Ensure the file exists and has the correct permissions";
        "Use relative paths from the current working directory";
      ]
    ()
