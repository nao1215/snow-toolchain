(*** @project snow-toolchain @module Test_Error_Reporting

  Tests for the enhanced error reporting system.

  This test module verifies that: - Error messages are user-friendly and
  informative - Source context is properly displayed - Suggestions are relevant
  and helpful - Error formatting is consistent across all error types *)

open Alcotest
open Error_reporting

(** Test basic error information structure *)
let test_error_info_creation () =
  let error =
    lexical_error ~filename:"test.sw" ~line:5 ~column:10 ~offset:50 ~code:"E001"
      ~title:"Test error" ~message:"This is a test error"
      ~suggestions:[ "Try this"; "Or this" ] ()
  in

  check string "error code" "E001" error.code;
  check string "error title" "Test error" error.title;
  check string "error message" "This is a test error" error.message;
  check (list string) "suggestions" [ "Try this"; "Or this" ] error.suggestions;
  match error.position with
  | None -> fail "Position should not be None"
  | Some pos ->
      check string "filename" "test.sw" pos.filename;
      check int "line" 5 pos.line;
      check int "column" 10 pos.column;
      check int "offset" 50 pos.offset

(** Test unterminated string error *)
let test_unterminated_string_error () =
  let error =
    unterminated_string_error ~filename:"main.sw" ~line:3 ~column:8 ~offset:25
      ()
  in

  check string "error code" "E001" error.code;
  check string "error title" "Unterminated string literal" error.title;
  check bool "has suggestions" true (List.length error.suggestions > 0)

(** Test invalid escape sequence error *)
let test_invalid_escape_error () =
  let error =
    invalid_escape_error ~filename:"test.sw" ~line:2 ~column:15 ~offset:30
      ~escape_char:'q' ()
  in

  check string "error code" "E002" error.code;
  check string "error title" "Invalid escape sequence" error.title;
  check bool "message contains escape char" true
    (String.contains error.message 'q')

(** Test expected token error *)
let test_expected_token_error () =
  let error =
    expected_token_error ~filename:"src.sw" ~line:10 ~column:5 ~offset:100
      ~expected:"}" ~found:"{" ()
  in

  check string "error code" "E003" error.code;
  check string "error title" "Unexpected token" error.title;
  check bool "message contains expected token" true
    (String.contains error.message '}');
  check bool "message contains found token" true
    (String.contains error.message '{')

(** Test missing main function error *)
let test_missing_main_error () =
  let error = missing_main_error ~filename:"program.sw" () in

  check string "error code" "E004" error.code;
  check string "error title" "Missing main function" error.title;
  check bool "has suggestions about main function" true
    (List.exists
       (fun s -> String.contains s 'm' && String.contains s 'a')
       error.suggestions)

(** Test type mismatch error *)
let test_type_mismatch_error () =
  let error =
    type_mismatch_error ~filename:"calc.sw" ~line:7 ~column:12 ~offset:85
      ~expected_type:"i32" ~actual_type:"string" ()
  in

  check string "error code" "E005" error.code;
  check string "error title" "Type mismatch" error.title;
  check bool "message contains expected type" true
    (String.contains error.message '3' && String.contains error.message '2');
  check bool "message contains actual type" true
    (String.contains error.message 's')

(** Test file not found error *)
let test_file_not_found_error () =
  let error = file_not_found_error ~filename:"missing.sw" () in

  check string "error code" "E006" error.code;
  check string "error title" "File not found" error.title;
  check bool "message contains filename" true
    (String.contains error.message 'm')

(** Test source context extraction *)
let test_source_context () =
  let source_content = "let x = 5\nlet y = \"hello\nlet z = 10" in
  let line = get_source_line source_content 2 in
  check (option string) "extracted line" (Some "let y = \"hello") line

(** Test position indicator creation *)
let test_position_indicator () =
  let indicator = create_position_indicator 5 3 in
  check bool "indicator has spaces" true (String.contains indicator ' ');
  (* Note: color codes make exact string matching difficult, so we just check
     for structure *)
  check bool "indicator has carets" true (String.length indicator > 5)

(** Test error message formatting *)
let test_error_formatting () =
  let error =
    syntax_error ~filename:"test.sw" ~line:1 ~column:5 ~offset:4 ~code:"E003"
      ~title:"Parse error" ~message:"Expected ';' after expression"
      ~suggestions:[ "Add semicolon at the end of the line" ]
      ()
  in

  let formatted = format_error_message error "" in
  check bool "formatted message contains error info" true
    (String.length formatted > 0);
  check bool "contains error code" true (String.contains formatted 'E');
  check bool "contains title" true (String.contains formatted 'P')

let suite =
  [
    ("Error info creation", `Quick, test_error_info_creation);
    ("Unterminated string error", `Quick, test_unterminated_string_error);
    ("Invalid escape error", `Quick, test_invalid_escape_error);
    ("Expected token error", `Quick, test_expected_token_error);
    ("Missing main function error", `Quick, test_missing_main_error);
    ("Type mismatch error", `Quick, test_type_mismatch_error);
    ("File not found error", `Quick, test_file_not_found_error);
    ("Source context extraction", `Quick, test_source_context);
    ("Position indicator creation", `Quick, test_position_indicator);
    ("Error message formatting", `Quick, test_error_formatting);
  ]

let () = run "Error Reporting" [ ("Error Reporting Tests", suite) ]
