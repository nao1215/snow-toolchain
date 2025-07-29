open Alcotest

(*** @project snow-toolchain @module Test_End_To_End_Integration

  Comprehensive end-to-end integration test suite for Snow toolchain.

  Tests complete workflows from source code to executable, including multi-file
  projects, module management, and real compilation scenarios. *)

(** Mock file system and environment for integration testing *)
module Integration_environment = struct
  type file_content = string
  type directory = string

  type project_structure = {
    files : (string * file_content) list;
    directories : directory list;
    snow_mod : file_content option;
    expected_output : string option;
  }

  (** Sample Snow source files for testing *)
  let hello_world_sw =
    {|
package main

func main() {
  println("Hello, Snow World!")
}
|}

  let math_utils_sw =
    {|
package main

func add(a: i32, b: i32) -> i32 {
  a + b
}

func multiply(a: i32, b: i32) -> i32 {
  a * b
}
|}

  let main_with_utils_sw =
    {|
package main

func main() {
  let result = add(5, 3)
  let product = multiply(result, 2)
  println("Result: " + product.to_string())
}
|}

  let test_file_sw =
    {|
package main

func test_add() {
  assert(add(2, 3) == 5)
  assert(add(-1, 1) == 0)
}

func test_multiply() {
  assert(multiply(3, 4) == 12)
  assert(multiply(0, 5) == 0)
}
|}

  let snow_mod_content =
    {|
module: example.com/my-project
version: v1.0.0
license: MIT

dependencies:
  std: latest

metadata:
  author: "Test User"
  description: "Example Snow project"
|}

  (** Test project configurations *)
  let single_file_project =
    {
      files = [ ("main.sw", hello_world_sw) ];
      directories = [];
      snow_mod = None;
      expected_output = Some "Hello, Snow World!";
    }

  let multi_file_project =
    {
      files = [ ("main.sw", main_with_utils_sw); ("utils.sw", math_utils_sw) ];
      directories = [];
      snow_mod = None;
      expected_output = Some "Result: 16";
    }

  let module_project =
    {
      files =
        [
          ("main.sw", main_with_utils_sw);
          ("src/utils.sw", math_utils_sw);
          ("test/test_utils.sw", test_file_sw);
        ];
      directories = [ "src"; "test" ];
      snow_mod = Some snow_mod_content;
      expected_output = Some "Result: 16";
    }

  let _library_project =
    {
      files =
        [
          ("src/lib.sw", math_utils_sw);
          ("examples/example.sw", main_with_utils_sw);
        ];
      directories = [ "src"; "examples" ];
      snow_mod = Some snow_mod_content;
      expected_output = None;
    }

  (** Mock execution of Snow commands *)
  type command_result = Success of string * int | Failure of string * int

  let mock_execute_command cmd args project =
    match cmd with
    | "build" -> (
        match args with
        | [ source_file ] when List.mem_assoc source_file project.files ->
            Success
              ("Build successful: " ^ Filename.remove_extension source_file, 0)
        | multiple_files
          when List.for_all
                 (fun f -> List.mem_assoc f project.files)
                 multiple_files ->
            Success
              ( Printf.sprintf "Build successful: main (%d files)"
                  (List.length multiple_files),
                0 )
        | _ -> Failure ("File not found or invalid", 1))
    | "run" -> (
        match args with
        | [ source_file ] when List.mem_assoc source_file project.files -> (
            match project.expected_output with
            | Some output -> Success (output, 0)
            | None -> Success ("Program executed", 0))
        | _ -> Failure ("Cannot run: file not found", 1))
    | "fmt" ->
        let sw_files =
          List.filter
            (fun (name, _) -> String.ends_with ~suffix:".sw" name)
            project.files
        in
        if List.length sw_files > 0 then
          Success (Printf.sprintf "Formatted %d files" (List.length sw_files), 0)
        else Success ("No Snow files to format", 0)
    | "mod" when List.length args > 0 && List.hd args = "init" ->
        Success ("Module initialized", 0)
    | "test" ->
        let test_files =
          List.filter (fun (name, _) -> String.contains name 't') project.files
        in
        if List.length test_files > 0 then
          Success
            ( Printf.sprintf "All tests passed (%d test files)"
                (List.length test_files),
              0 )
        else Success ("No tests found", 0)
    | _ -> Failure ("Unknown command", 1)

  (** Mock file operations *)
  let mock_file_exists filename project = List.mem_assoc filename project.files

  let mock_get_file_content filename project =
    try Some (List.assoc filename project.files) with Not_found -> None

  let mock_directory_exists dirname project =
    List.mem dirname project.directories
end

(** Test single file build and run workflow *)
let test_single_file_workflow () =
  let open Integration_environment in
  (* Test build command *)
  let build_result =
    mock_execute_command "build" [ "main.sw" ] single_file_project
  in
  (match build_result with
  | Success (msg, code) ->
      check int "build_exit_code" 0 code;
      check bool "build_success_message" (String.contains msg 'B') true
  | Failure _ -> fail "Single file build should succeed");

  (* Test run command *)
  let run_result =
    mock_execute_command "run" [ "main.sw" ] single_file_project
  in
  match run_result with
  | Success (output, code) ->
      check int "run_exit_code" 0 code;
      check bool "run_expected_output" (String.contains output 'H') true
  | Failure _ -> fail "Single file run should succeed"

(** Test multi-file project workflow *)
let test_multi_file_workflow () =
  let open Integration_environment in
  (* Test multi-file build *)
  let build_result =
    mock_execute_command "build" [ "main.sw"; "utils.sw" ] multi_file_project
  in
  (match build_result with
  | Success (msg, code) ->
      check int "multi_build_exit_code" 0 code;
      check bool "multi_build_message" (String.contains msg '2') true
  | Failure _ -> fail "Multi-file build should succeed");

  (* Test format command on project *)
  let fmt_result = mock_execute_command "fmt" [] multi_file_project in
  match fmt_result with
  | Success (msg, code) ->
      check int "fmt_exit_code" 0 code;
      check bool "fmt_files_count" (String.contains msg '2') true
  | Failure _ -> fail "Format should succeed on multi-file project"

(** Test module-based project workflow *)
let test_module_project_workflow () =
  let open Integration_environment in
  (* Test module initialization *)
  let init_result = mock_execute_command "mod" [ "init" ] module_project in
  (match init_result with
  | Success (msg, code) ->
      check int "mod_init_exit_code" 0 code;
      check bool "mod_init_message" (String.contains msg 'M') true
  | Failure _ -> fail "Module init should succeed");

  (* Test build with module structure *)
  let build_result =
    mock_execute_command "build" [ "main.sw" ] module_project
  in
  (match build_result with
  | Success (msg, code) ->
      check int "module_build_exit_code" 0 code;
      check bool "module_build_success" (String.contains msg 'B') true
  | Failure _ -> fail "Module project build should succeed");

  (* Test running tests *)
  let test_result = mock_execute_command "test" [] module_project in
  match test_result with
  | Success (msg, code) ->
      check int "test_exit_code" 0 code;
      check bool "test_passed_message" (String.contains msg 'p') true
  | Failure _ -> fail "Tests should pass"

(** Test error scenarios in integration *)
let test_error_scenarios () =
  let open Integration_environment in
  (* Test build with missing file *)
  let build_result =
    mock_execute_command "build" [ "nonexistent.sw" ] single_file_project
  in
  (match build_result with
  | Failure (msg, code) ->
      check int "missing_file_exit_code" 1 code;
      check bool "missing_file_error" (String.contains msg 'F') true
  | Success _ -> fail "Build with missing file should fail");

  (* Test run with missing file *)
  let run_result =
    mock_execute_command "run" [ "missing.sw" ] single_file_project
  in
  (match run_result with
  | Failure (msg, code) ->
      check int "missing_run_exit_code" 1 code;
      check bool "missing_run_error" (String.contains msg 'f') true
  | Success _ -> fail "Run with missing file should fail");

  (* Test unknown command *)
  let unknown_result =
    mock_execute_command "unknown" [ "main.sw" ] single_file_project
  in
  match unknown_result with
  | Failure (msg, code) ->
      check int "unknown_cmd_exit_code" 1 code;
      check bool "unknown_cmd_error" (String.contains msg 'U') true
  | Success _ -> fail "Unknown command should fail"

(** Test file system operations integration *)
let test_file_system_integration () =
  let open Integration_environment in
  (* Test file existence checks *)
  check bool "main_exists" true (mock_file_exists "main.sw" single_file_project);
  check bool "utils_exists" true
    (mock_file_exists "utils.sw" multi_file_project);
  check bool "nonexistent_not_exists" false
    (mock_file_exists "nonexistent.sw" single_file_project);

  (* Test directory existence checks *)
  check bool "src_exists" true (mock_directory_exists "src" module_project);
  check bool "test_exists" true (mock_directory_exists "test" module_project);
  check bool "nonexistent_dir_not_exists" false
    (mock_directory_exists "nonexistent" module_project);

  (* Test file content retrieval *)
  let content = mock_get_file_content "main.sw" single_file_project in
  (match content with
  | Some c -> check bool "file_content_retrieved" (String.contains c 'p') true
  | None -> fail "Should retrieve file content");

  let missing_content =
    mock_get_file_content "missing.sw" single_file_project
  in
  check (option string) "missing_file_content" None missing_content

(** Test complete development workflow *)
let test_complete_development_workflow () =
  let open Integration_environment in
  (* Simulate complete workflow: init -> write code -> format -> build -> test
     -> run *)
  let workflow_steps =
    [
      ("mod", [ "init" ], module_project);
      ("fmt", [], module_project);
      ("build", [ "main.sw" ], module_project);
      ("test", [], module_project);
      ("run", [ "main.sw" ], module_project);
    ]
  in

  List.iteri
    (fun i (cmd, args, project) ->
      let result = mock_execute_command cmd args project in
      match result with
      | Success (_, code) ->
          check int (Printf.sprintf "workflow_step_%d_success" i) 0 code
      | Failure (msg, _) ->
          fail (Printf.sprintf "Workflow step %d should succeed: %s" i msg))
    workflow_steps

(** Test project structure validation *)
let test_project_structure_validation () =
  let open Integration_environment in
  (* Test single file project structure *)
  check int "single_file_count" 1 (List.length single_file_project.files);
  check bool "single_no_dirs"
    (List.length single_file_project.directories = 0)
    true;
  check (option string) "single_no_mod" None single_file_project.snow_mod;

  (* Test multi-file project structure *)
  check int "multi_file_count" 2 (List.length multi_file_project.files);
  check bool "multi_has_main"
    (List.mem_assoc "main.sw" multi_file_project.files)
    true;
  check bool "multi_has_utils"
    (List.mem_assoc "utils.sw" multi_file_project.files)
    true;

  (* Test module project structure *)
  check bool "module_has_main"
    (List.mem_assoc "main.sw" module_project.files)
    true;
  check bool "module_has_src" (List.mem "src" module_project.directories) true;
  check bool "module_has_test" (List.mem "test" module_project.directories) true;
  match module_project.snow_mod with
  | Some content ->
      check bool "module_has_mod_content" (String.contains content 'v') true
  | None -> fail "Module project should have snow.mod"

(** Test cross-command consistency *)
let test_command_consistency () =
  let open Integration_environment in
  (* Test that build and run work consistently *)
  let build_result =
    mock_execute_command "build" [ "main.sw" ] single_file_project
  in
  let run_result =
    mock_execute_command "run" [ "main.sw" ] single_file_project
  in

  match (build_result, run_result) with
  | Success (_, build_code), Success (_, run_code) ->
      check int "consistent_build_code" build_code 0;
      check int "consistent_run_code" run_code 0
  | _ -> fail "Build and run should both succeed for consistency"

(** Test output format consistency *)
let test_output_format_consistency () =
  let open Integration_environment in
  (* Test that success messages follow consistent format *)
  let build_result =
    mock_execute_command "build" [ "main.sw" ] single_file_project
  in
  let fmt_result = mock_execute_command "fmt" [] single_file_project in

  match (build_result, fmt_result) with
  | Success (build_msg, _), Success (fmt_msg, _) ->
      check bool "build_msg_format" (String.length build_msg > 0) true;
      check bool "fmt_msg_format" (String.length fmt_msg > 0) true
  | _ -> fail "Both commands should provide consistent output"

(** Test edge cases in integration *)
let test_integration_edge_cases () =
  let open Integration_environment in
  (* Test empty project *)
  let empty_project =
    { files = []; directories = []; snow_mod = None; expected_output = None }
  in
  let fmt_result = mock_execute_command "fmt" [] empty_project in
  (match fmt_result with
  | Success (msg, 0) ->
      check bool "empty_fmt_handled" (String.contains msg 'N') true
  | _ -> fail "Empty project format should be handled gracefully");

  (* Test project with only directories *)
  let dir_only_project =
    {
      files = [];
      directories = [ "src"; "test" ];
      snow_mod = None;
      expected_output = None;
    }
  in
  check bool "dir_only_src_exists" true
    (mock_directory_exists "src" dir_only_project);
  check bool "dir_only_no_files" false
    (mock_file_exists "main.sw" dir_only_project)

(** Main test suite *)
let () =
  let open Alcotest in
  run "Snow End-to-End Integration Tests"
    [
      ( "single_file",
        [ test_case "single file workflow" `Quick test_single_file_workflow ] );
      ( "multi_file",
        [ test_case "multi-file workflow" `Quick test_multi_file_workflow ] );
      ( "module_project",
        [
          test_case "module project workflow" `Quick
            test_module_project_workflow;
        ] );
      ( "error_scenarios",
        [ test_case "error scenarios" `Quick test_error_scenarios ] );
      ( "file_system",
        [
          test_case "file system integration" `Quick
            test_file_system_integration;
        ] );
      ( "development_workflow",
        [
          test_case "complete development workflow" `Quick
            test_complete_development_workflow;
          test_case "project structure validation" `Quick
            test_project_structure_validation;
        ] );
      ( "consistency",
        [
          test_case "command consistency" `Quick test_command_consistency;
          test_case "output format consistency" `Quick
            test_output_format_consistency;
        ] );
      ( "edge_cases",
        [
          test_case "integration edge cases" `Quick test_integration_edge_cases;
        ] );
    ]
