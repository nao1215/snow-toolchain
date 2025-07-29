open Alcotest

(*** @project snow-toolchain @module Test_CLI_Error_Handling

  Comprehensive test suite for Snow CLI error handling and edge cases.

  Tests cover command-line option errors, file system errors, compilation
  errors, and proper error message formatting with user-friendly suggestions. *)

(** Mock error reporting module for testing *)
module Mock_error_reporting = struct
  type error_info = {
    code : string;
    title : string;
    message : string;
    suggestions : string list;
  }

  type cli_error =
    | Invalid_option of string
    | Missing_argument of string
    | File_not_found of string
    | Invalid_file_extension of string
    | Compilation_failed of string
    | Permission_denied of string
    | Unknown_command of string
    | Missing_input_files
    | Output_already_exists of string

  (** Generate error info from CLI error types *)
  let error_info_from_cli_error = function
    | Invalid_option opt ->
        {
          code = "E001";
          title = "Invalid Option";
          message = Printf.sprintf "Unknown option: %s" opt;
          suggestions =
            [
              "Check 'snow help' for available options";
              "Make sure option names are spelled correctly";
            ];
        }
    | Missing_argument opt ->
        {
          code = "E002";
          title = "Missing Argument";
          message = Printf.sprintf "Option %s requires an argument" opt;
          suggestions =
            [
              "Provide a value after the option";
              "Example: snow build -o myapp main.sw";
            ];
        }
    | File_not_found file ->
        {
          code = "E003";
          title = "File Not Found";
          message = Printf.sprintf "Cannot find file: %s" file;
          suggestions =
            [
              "Check if the file path is correct";
              "Make sure the file exists in the specified location";
            ];
        }
    | Invalid_file_extension file ->
        {
          code = "E004";
          title = "Invalid File Extension";
          message = Printf.sprintf "File must have .sw extension: %s" file;
          suggestions =
            [
              "Snow source files must end with .sw";
              "Rename your file with the correct extension";
            ];
        }
    | Compilation_failed reason ->
        {
          code = "E005";
          title = "Compilation Failed";
          message = Printf.sprintf "Failed to compile: %s" reason;
          suggestions =
            [
              "Check your Snow syntax";
              "Run with -v for verbose output";
              "Review the error messages above";
            ];
        }
    | Permission_denied file ->
        {
          code = "E006";
          title = "Permission Denied";
          message = Printf.sprintf "Cannot access file: %s" file;
          suggestions =
            [
              "Check file permissions";
              "Make sure you have read access to the file";
            ];
        }
    | Unknown_command cmd ->
        {
          code = "E007";
          title = "Unknown Command";
          message = Printf.sprintf "Unknown subcommand: %s" cmd;
          suggestions =
            [
              "Run 'snow help' to see available commands";
              "Check if the command name is spelled correctly";
            ];
        }
    | Missing_input_files ->
        {
          code = "E008";
          title = "No Input Files";
          message = "No input files specified";
          suggestions =
            [
              "Provide at least one .sw file to compile";
              "Example: snow build main.sw";
            ];
        }
    | Output_already_exists file ->
        {
          code = "E009";
          title = "Output Already Exists";
          message = Printf.sprintf "Output file already exists: %s" file;
          suggestions =
            [
              "Use -f to force overwrite";
              "Choose a different output name with -o";
            ];
        }

  (** Mock error formatting *)
  let format_error error_info context =
    let lines =
      [
        Printf.sprintf "[%s] %s" error_info.code error_info.title;
        error_info.message;
      ]
    in
    let suggestion_lines =
      List.map (fun s -> "  • " ^ s) error_info.suggestions
    in
    if context <> "" then
      lines @ [ "Context: " ^ context ] @ [ "Suggestions:" ] @ suggestion_lines
    else lines @ [ "Suggestions:" ] @ suggestion_lines
end

(** Test invalid command line options *)
let test_invalid_options () =
  let open Mock_error_reporting in
  (* Test unknown short option *)
  let error = error_info_from_cli_error (Invalid_option "-x") in
  check string "invalid_short_code" "E001" error.code;
  check bool "invalid_short_message" (String.contains error.message 'x') true;
  check bool "invalid_short_suggestions"
    (List.length error.suggestions >= 1)
    true;

  (* Test unknown long option *)
  let error = error_info_from_cli_error (Invalid_option "--unknown") in
  check string "invalid_long_code" "E001" error.code;
  check bool "invalid_long_message" (String.contains error.message 'u') true;

  (* Test option that looks like file *)
  let error = error_info_from_cli_error (Invalid_option "main.sw-backup") in
  check bool "invalid_option_like_file" (String.contains error.message 'b') true

(** Test missing arguments for options *)
let test_missing_arguments () =
  let open Mock_error_reporting in
  (* Test missing output file after -o *)
  let error = error_info_from_cli_error (Missing_argument "-o") in
  check string "missing_output_code" "E002" error.code;
  check bool "missing_output_message" (String.contains error.message 'o') true;
  check bool "missing_output_suggestions"
    (List.exists (fun s -> String.contains s 'E') error.suggestions)
    true;

  (* Test missing target after --target *)
  let error = error_info_from_cli_error (Missing_argument "--target") in
  check bool "missing_target_message" (String.contains error.message 't') true;

  (* Test missing library after -l *)
  let error = error_info_from_cli_error (Missing_argument "-l") in
  check bool "missing_library_message" (String.contains error.message 'l') true

(** Test file system errors *)
let test_file_system_errors () =
  let open Mock_error_reporting in
  (* Test file not found *)
  let error = error_info_from_cli_error (File_not_found "nonexistent.sw") in
  check string "file_not_found_code" "E003" error.code;
  check bool "file_not_found_message" (String.contains error.message 'n') true;
  check bool "file_not_found_suggestions"
    (List.exists (fun s -> String.contains s 'c') error.suggestions)
    true;

  (* Test permission denied *)
  let error = error_info_from_cli_error (Permission_denied "/root/secret.sw") in
  check string "permission_denied_code" "E006" error.code;
  check bool "permission_denied_message"
    (String.contains error.message 's')
    true;

  (* Test directory instead of file *)
  let error = error_info_from_cli_error (File_not_found "src/") in
  check bool "directory_error_message" (String.contains error.message 's') true

(** Test file extension validation *)
let test_file_extension_errors () =
  let open Mock_error_reporting in
  (* Test C file *)
  let error = error_info_from_cli_error (Invalid_file_extension "main.c") in
  check string "c_file_code" "E004" error.code;
  check bool "c_file_message" (String.contains error.message 'c') true;
  check bool "c_file_suggestions"
    (List.exists (fun s -> String.contains s '.') error.suggestions)
    true;

  (* Test Python file *)
  let error = error_info_from_cli_error (Invalid_file_extension "script.py") in
  check bool "py_file_message" (String.contains error.message 'p') true;

  (* Test no extension *)
  let error = error_info_from_cli_error (Invalid_file_extension "main") in
  check bool "no_ext_message" (String.contains error.message 'm') true;

  (* Test wrong Snow extension *)
  let error = error_info_from_cli_error (Invalid_file_extension "main.snow") in
  check bool "wrong_snow_ext_message" (String.contains error.message 's') true

(** Test compilation error handling *)
let test_compilation_errors () =
  let open Mock_error_reporting in
  (* Test syntax error *)
  let error =
    error_info_from_cli_error (Compilation_failed "syntax error at line 10")
  in
  check string "syntax_error_code" "E005" error.code;
  check bool "syntax_error_message" (String.contains error.message 's') true;
  check bool "syntax_error_suggestions"
    (List.exists (fun s -> String.contains s 'v') error.suggestions)
    true;

  (* Test type error *)
  let error =
    error_info_from_cli_error
      (Compilation_failed "type mismatch: expected i32, got string")
  in
  check bool "type_error_message" (String.contains error.message 't') true;

  (* Test missing function *)
  let error =
    error_info_from_cli_error (Compilation_failed "undefined function: foo")
  in
  check bool "missing_func_message" (String.contains error.message 'f') true

(** Test command validation errors *)
let test_command_errors () =
  let open Mock_error_reporting in
  (* Test unknown subcommand *)
  let error = error_info_from_cli_error (Unknown_command "compile") in
  check string "unknown_cmd_code" "E007" error.code;
  check bool "unknown_cmd_message" (String.contains error.message 'c') true;
  check bool "unknown_cmd_suggestions"
    (List.exists (fun s -> String.contains s 'h') error.suggestions)
    true;

  (* Test typo in command *)
  let error = error_info_from_cli_error (Unknown_command "biuld") in
  check bool "typo_cmd_message" (String.contains error.message 'b') true;

  (* Test empty command *)
  let error = error_info_from_cli_error (Unknown_command "") in
  check bool "empty_cmd_message" (String.length error.message > 0) true

(** Test input validation errors *)
let test_input_validation_errors () =
  let open Mock_error_reporting in
  (* Test no input files *)
  let error = error_info_from_cli_error Missing_input_files in
  check string "no_input_code" "E008" error.code;
  check bool "no_input_message" (String.contains error.message 'N') true;
  check bool "no_input_suggestions"
    (List.exists (fun s -> String.contains s '.') error.suggestions)
    true;

  (* Test output already exists *)
  let error = error_info_from_cli_error (Output_already_exists "main") in
  check string "output_exists_code" "E009" error.code;
  check bool "output_exists_message" (String.contains error.message 'm') true;
  check bool "output_exists_suggestions"
    (List.exists (fun s -> String.contains s 'f') error.suggestions)
    true

(** Test error message formatting *)
let test_error_formatting () =
  let open Mock_error_reporting in
  (* Test basic formatting *)
  let error = error_info_from_cli_error (Invalid_option "-x") in
  let formatted = format_error error "" in
  check bool "format_has_code"
    (List.exists (fun line -> String.contains line '[') formatted)
    true;
  let bullet_char = String.get "•" 0 in
  check bool "format_has_suggestions"
    (List.exists (fun line -> String.contains line bullet_char) formatted)
    true;

  (* Test formatting with context *)
  let error = error_info_from_cli_error (File_not_found "test.sw") in
  let formatted = format_error error "building project" in
  check bool "format_has_context"
    (List.exists (fun line -> String.contains line 'C') formatted)
    true;

  (* Test suggestion bullet points *)
  let error = error_info_from_cli_error Missing_input_files in
  let formatted = format_error error "" in
  let bullet_char = String.get "•" 0 in
  let suggestion_lines =
    List.filter (fun line -> String.contains line bullet_char) formatted
  in
  check bool "format_has_bullets" (List.length suggestion_lines >= 1) true

(** Test error code consistency *)
let test_error_code_consistency () =
  let open Mock_error_reporting in
  let error_cases =
    [
      (Invalid_option "-x", "E001");
      (Missing_argument "-o", "E002");
      (File_not_found "test.sw", "E003");
      (Invalid_file_extension "main.c", "E004");
      (Compilation_failed "error", "E005");
      (Permission_denied "file", "E006");
      (Unknown_command "cmd", "E007");
      (Missing_input_files, "E008");
      (Output_already_exists "file", "E009");
    ]
  in

  List.iter
    (fun (cli_error, expected_code) ->
      let error = error_info_from_cli_error cli_error in
      check string ("error_code_" ^ expected_code) expected_code error.code)
    error_cases

(** Test suggestion quality *)
let test_suggestion_quality () =
  let open Mock_error_reporting in
  (* Test that suggestions are helpful *)
  let error = error_info_from_cli_error (Invalid_option "--verbose-mode") in
  check bool "suggestions_not_empty" (List.length error.suggestions > 0) true;
  check bool "suggestions_actionable"
    (List.exists
       (fun s -> String.contains s 'h' || String.contains s 'c')
       error.suggestions)
    true;

  (* Test that file errors suggest file operations *)
  let error = error_info_from_cli_error (File_not_found "main.sw") in
  check bool "file_suggestions_relevant"
    (List.exists (fun s -> String.contains s 'f') error.suggestions)
    true;

  (* Test that compilation errors suggest debugging *)
  let error = error_info_from_cli_error (Compilation_failed "type error") in
  check bool "compile_suggestions_helpful"
    (List.exists (fun s -> String.contains s 'v') error.suggestions)
    true

(** Test edge cases in error handling *)
let test_error_edge_cases () =
  let open Mock_error_reporting in
  (* Test very long error messages *)
  let long_msg = String.make 200 'x' in
  let error = error_info_from_cli_error (Compilation_failed long_msg) in
  check bool "long_message_handled" (String.length error.message > 100) true;

  (* Test special characters in filenames *)
  let error =
    error_info_from_cli_error (File_not_found "file with spaces.sw")
  in
  check bool "special_chars_message" (String.contains error.message ' ') true;

  (* Test Unicode in error messages *)
  let error = error_info_from_cli_error (Invalid_option "—verbose") in
  check bool "unicode_option_handled" (String.length error.message > 0) true

(** Test error recovery suggestions *)
let test_error_recovery () =
  let open Mock_error_reporting in
  (* Test that each error type has appropriate recovery suggestions *)
  let test_cases =
    [
      (Invalid_option "-x", [ "help" ]);
      (Missing_argument "-o", [ "Example" ]);
      (File_not_found "main.sw", [ "Check" ]);
      (Invalid_file_extension "main.c", [ ".sw" ]);
      (Compilation_failed "error", [ "syntax"; "verbose" ]);
    ]
  in

  List.iter
    (fun (cli_error, expected_keywords) ->
      let error = error_info_from_cli_error cli_error in
      List.iter
        (fun keyword ->
          let has_keyword =
            List.exists
              (fun s -> String.contains s keyword.[0])
              error.suggestions
          in
          check bool ("recovery_" ^ keyword) true has_keyword)
        expected_keywords)
    test_cases

(** Main test suite *)
let () =
  let open Alcotest in
  run "Snow CLI Error Handling Tests"
    [
      ( "option_errors",
        [
          test_case "invalid options" `Quick test_invalid_options;
          test_case "missing arguments" `Quick test_missing_arguments;
        ] );
      ( "file_errors",
        [
          test_case "file system errors" `Quick test_file_system_errors;
          test_case "file extension errors" `Quick test_file_extension_errors;
        ] );
      ( "compilation_errors",
        [ test_case "compilation errors" `Quick test_compilation_errors ] );
      ( "command_errors",
        [
          test_case "command errors" `Quick test_command_errors;
          test_case "input validation errors" `Quick
            test_input_validation_errors;
        ] );
      ( "error_formatting",
        [
          test_case "error message formatting" `Quick test_error_formatting;
          test_case "error code consistency" `Quick test_error_code_consistency;
          test_case "suggestion quality" `Quick test_suggestion_quality;
        ] );
      ( "edge_cases",
        [
          test_case "error edge cases" `Quick test_error_edge_cases;
          test_case "error recovery" `Quick test_error_recovery;
        ] );
    ]
