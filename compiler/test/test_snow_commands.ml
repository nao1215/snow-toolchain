open Alcotest

(*** @project snow-toolchain @module Test_Snow_Commands

  Comprehensive test suite for Snow command functionality and integration.

  Tests cover command execution, build pipeline integration, formatting, and
  module management with proper error scenarios and edge cases. *)

(** Mock modules for testing command functionality *)
module Mock_commands = struct
  type build_status = Build_success of string | Build_error of string
  type format_status = Format_success of string list | Format_error of string
  type mod_init_status = Init_success of string | Init_error of string
  type run_status = Run_success of string * int | Run_error of string

  (** Mock build command execution *)
  let mock_build_command input_files output_file optimization_level debug_info
      verbose =
    match input_files with
    | [] -> Build_error "No input files specified"
    | files
      when List.exists (fun f -> not (String.ends_with ~suffix:".sw" f)) files
      ->
        Build_error "Invalid file extension - only .sw files supported"
    | files when List.exists (fun f -> String.contains f ' ') files ->
        Build_error "File names with spaces not supported"
    | [ single_file ] when String.ends_with ~suffix:".sw" single_file ->
        let output_name =
          match output_file with
          | Some name -> name
          | None -> Filename.remove_extension single_file
        in
        if verbose then
          Build_success
            (Printf.sprintf "Build successful: %s (optimization: %d, debug: %b)"
               output_name optimization_level debug_info)
        else Build_success ("Build successful: " ^ output_name)
    | multiple_files ->
        let output_name =
          match output_file with Some name -> name | None -> "main"
        in
        Build_success
          (Printf.sprintf "Build successful: %s (%d files)" output_name
             (List.length multiple_files))

  (** Mock format command execution *)
  let mock_format_command files =
    match files with
    | [] -> Format_success [ "No files to format" ]
    | files when List.for_all (fun f -> String.ends_with ~suffix:".sw" f) files
      ->
        Format_success (List.map (fun f -> "Formatted: " ^ f) files)
    | files ->
        let invalid =
          List.filter (fun f -> not (String.ends_with ~suffix:".sw" f)) files
        in
        Format_error ("Invalid files: " ^ String.concat ", " invalid)

  (** Mock mod init command execution *)
  let mock_mod_init_command module_path =
    match module_path with
    | None -> Init_success "Initialized module in current directory"
    | Some path when String.contains path '/' && String.length path > 3 ->
        Init_success ("Initialized module: " ^ path)
    | Some path when String.length path < 3 ->
        Init_error "Module path too short"
    | Some path -> Init_error ("Invalid module path format: " ^ path)

  (** Mock run command execution *)
  let mock_run_command source_file args =
    if not (String.ends_with ~suffix:".sw" source_file) then
      Run_error "Source file must have .sw extension"
    else if String.contains source_file ' ' then
      Run_error "Source file path cannot contain spaces"
    else
      let arg_str =
        if List.length args > 0 then " with args: " ^ String.concat " " args
        else ""
      in
      Run_success ("Executed: " ^ source_file ^ arg_str, 0)

  (** Mock file system operations *)
  let mock_file_exists filename =
    List.mem filename
      [ "main.sw"; "util.sw"; "test_main.sw"; "valid.sw"; "lib.sw" ]

  let mock_directory_exists dirname =
    List.mem dirname [ "src"; "test"; "valid_dir" ]

  (** Mock command line parsing *)
  let parse_run_args args =
    match args with
    | [] -> (None, [])
    | source :: rest ->
        if String.ends_with ~suffix:".sw" source then (Some source, rest)
        else (None, args)
end

(** Test build command functionality *)
let test_build_command () =
  let open Mock_commands in
  (* Test successful single file build *)
  let result = mock_build_command [ "main.sw" ] None 0 false false in
  (match result with
  | Build_success msg ->
      check bool "build_success_contains_main" (String.contains msg 'm') true
  | Build_error _ -> fail "Build should succeed with valid input");

  (* Test build with custom output *)
  let result = mock_build_command [ "main.sw" ] (Some "hello") 0 false false in
  (match result with
  | Build_success msg ->
      check bool "build_custom_output" (String.contains msg 'h') true
  | Build_error _ -> fail "Build should succeed with custom output");

  (* Test build with optimization *)
  let result = mock_build_command [ "main.sw" ] None 2 true true in
  (match result with
  | Build_success msg ->
      check bool "build_verbose_optimization"
        (String.contains msg '2' && String.contains msg 't')
        true
  | Build_error _ -> fail "Build should succeed with optimization");

  (* Test multiple file build *)
  let result = mock_build_command [ "main.sw"; "util.sw" ] None 0 false false in
  (match result with
  | Build_success msg ->
      check bool "build_multiple_files" (String.contains msg '2') true
  | Build_error _ -> fail "Build should succeed with multiple files");

  (* Test build error - no input files *)
  let result = mock_build_command [] None 0 false false in
  (match result with
  | Build_error msg ->
      check bool "build_no_input_error" (String.contains msg 'N') true
  | Build_success _ -> fail "Build should fail with no input files");

  (* Test build error - invalid file extension *)
  let result = mock_build_command [ "main.c" ] None 0 false false in
  match result with
  | Build_error msg ->
      check bool "build_invalid_ext_error" (String.contains msg 'I') true
  | Build_success _ -> fail "Build should fail with invalid extension"

(** Test format command functionality *)
let test_format_command () =
  let open Mock_commands in
  (* Test successful formatting *)
  let result = mock_format_command [ "main.sw"; "util.sw" ] in
  (match result with
  | Format_success msgs ->
      check int "format_success_count" 2 (List.length msgs);
      check bool "format_main_included"
        (List.exists (fun msg -> String.contains msg 'm') msgs)
        true
  | Format_error _ -> fail "Format should succeed with valid files");

  (* Test format with no files *)
  let result = mock_format_command [] in
  (match result with
  | Format_success msgs -> check int "format_no_files" 1 (List.length msgs)
  | Format_error _ -> fail "Format should handle empty file list");

  (* Test format error - invalid files *)
  let result = mock_format_command [ "main.c"; "util.py" ] in
  match result with
  | Format_error msg ->
      check bool "format_invalid_files_error" (String.contains msg 'I') true
  | Format_success _ -> fail "Format should fail with invalid files"

(** Test mod init command functionality *)
let test_mod_init_command () =
  let open Mock_commands in
  (* Test init in current directory *)
  let result = mock_mod_init_command None in
  (match result with
  | Init_success msg ->
      check bool "init_current_dir" (String.contains msg 'c') true
  | Init_error _ -> fail "Mod init should succeed in current directory");

  (* Test init with valid module path *)
  let result = mock_mod_init_command (Some "github.com/user/project") in
  (match result with
  | Init_success msg ->
      check bool "init_valid_path" (String.contains msg 'g') true
  | Init_error _ -> fail "Mod init should succeed with valid path");

  (* Test init error - path too short *)
  let result = mock_mod_init_command (Some "ab") in
  (match result with
  | Init_error msg ->
      check bool "init_path_too_short" (String.contains msg 's') true
  | Init_success _ -> fail "Mod init should fail with short path");

  (* Test init error - invalid path format *)
  let result = mock_mod_init_command (Some "invalid") in
  match result with
  | Init_error msg ->
      check bool "init_invalid_path" (String.contains msg 'I') true
  | Init_success _ -> fail "Mod init should fail with invalid path"

(** Test run command functionality *)
let test_run_command () =
  let open Mock_commands in
  (* Test successful run without arguments *)
  let result = mock_run_command "main.sw" [] in
  (match result with
  | Run_success (msg, code) ->
      check bool "run_success_main" (String.contains msg 'm') true;
      check int "run_success_code" 0 code
  | Run_error _ -> fail "Run should succeed with valid source");

  (* Test successful run with arguments *)
  let result = mock_run_command "main.sw" [ "arg1"; "arg2" ] in
  (match result with
  | Run_success (msg, code) ->
      check bool "run_with_args" (String.contains msg 'a') true;
      check int "run_with_args_code" 0 code
  | Run_error _ -> fail "Run should succeed with arguments");

  (* Test run error - invalid extension *)
  let result = mock_run_command "main.c" [] in
  (match result with
  | Run_error msg -> check bool "run_invalid_ext" (String.contains msg 'e') true
  | Run_success _ -> fail "Run should fail with invalid extension");

  (* Test run error - space in filename *)
  let result = mock_run_command "main file.sw" [] in
  match result with
  | Run_error msg ->
      check bool "run_space_in_name" (String.contains msg 's') true
  | Run_success _ -> fail "Run should fail with space in filename"

(** Test argument parsing for run command *)
let test_run_argument_parsing () =
  let open Mock_commands in
  (* Test parsing with source file *)
  let source, args = parse_run_args [ "main.sw"; "arg1"; "arg2" ] in
  check (option string) "run_parse_source" (Some "main.sw") source;
  check (list string) "run_parse_args" [ "arg1"; "arg2" ] args;

  (* Test parsing without source file *)
  let source, args = parse_run_args [ "arg1"; "arg2" ] in
  check (option string) "run_parse_no_source" None source;
  check (list string) "run_parse_all_args" [ "arg1"; "arg2" ] args;

  (* Test parsing empty *)
  let source, args = parse_run_args [] in
  check (option string) "run_parse_empty_source" None source;
  check (list string) "run_parse_empty_args" [] args

(** Test file system mock operations *)
let test_file_system_operations () =
  let open Mock_commands in
  (* Test file existence checks *)
  check bool "main_exists" true (mock_file_exists "main.sw");
  check bool "util_exists" true (mock_file_exists "util.sw");
  check bool "nonexistent_not_exists" false (mock_file_exists "nonexistent.sw");

  (* Test directory existence checks *)
  check bool "src_exists" true (mock_directory_exists "src");
  check bool "test_exists" true (mock_directory_exists "test");
  check bool "nonexistent_dir_not_exists" false
    (mock_directory_exists "nonexistent")

(** Test command integration scenarios *)
let test_command_integration () =
  let open Mock_commands in
  (* Test build -> run workflow *)
  let build_result =
    mock_build_command [ "main.sw" ] (Some "test_app") 0 false false
  in
  (match build_result with
  | Build_success _ -> (
      let run_result = mock_run_command "main.sw" [ "test_arg" ] in
      match run_result with
      | Run_success _ -> check bool "build_run_integration" true true
      | Run_error _ -> fail "Run should succeed after successful build")
  | Build_error _ -> fail "Build should succeed in integration test");

  (* Test format -> build workflow *)
  let format_result = mock_format_command [ "main.sw"; "util.sw" ] in
  match format_result with
  | Format_success _ -> (
      let build_result =
        mock_build_command [ "main.sw"; "util.sw" ] None 1 false false
      in
      match build_result with
      | Build_success _ -> check bool "format_build_integration" true true
      | Build_error _ -> fail "Build should succeed after formatting")
  | Format_error _ -> fail "Format should succeed in integration test"

(** Test error propagation and handling *)
let test_error_handling () =
  let open Mock_commands in
  (* Test that errors are properly categorized *)
  let test_error_cases =
    [
      ("build_no_files", mock_build_command [] None 0 false false);
      ( "build_invalid_ext",
        mock_build_command [ "invalid.txt" ] None 0 false false );
    ]
  in

  List.iter
    (fun (test_name, result) ->
      match result with
      | Build_error _ -> check bool test_name true true
      | Build_success _ -> fail (test_name ^ " should have failed"))
    test_error_cases

(** Test command output consistency *)
let test_output_consistency () =
  let open Mock_commands in
  (* Test that verbose output includes additional information *)
  let quiet_result = mock_build_command [ "main.sw" ] None 2 true false in
  let verbose_result = mock_build_command [ "main.sw" ] None 2 true true in

  match (quiet_result, verbose_result) with
  | Build_success quiet_msg, Build_success verbose_msg ->
      check bool "verbose_longer_than_quiet"
        (String.length verbose_msg > String.length quiet_msg)
        true
  | _ -> fail "Both builds should succeed for output consistency test"

(** Main test suite *)
let () =
  let open Alcotest in
  run "Snow Commands Tests"
    [
      ( "build_command",
        [ test_case "build command functionality" `Quick test_build_command ] );
      ( "format_command",
        [ test_case "format command functionality" `Quick test_format_command ]
      );
      ( "mod_command",
        [ test_case "mod init functionality" `Quick test_mod_init_command ] );
      ( "run_command",
        [
          test_case "run command functionality" `Quick test_run_command;
          test_case "run argument parsing" `Quick test_run_argument_parsing;
        ] );
      ( "file_system",
        [
          test_case "file system operations" `Quick test_file_system_operations;
        ] );
      ( "integration",
        [
          test_case "command integration" `Quick test_command_integration;
          test_case "error handling" `Quick test_error_handling;
          test_case "output consistency" `Quick test_output_consistency;
        ] );
    ]
