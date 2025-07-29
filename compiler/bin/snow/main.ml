(*** @project snow-toolchain @module Snow_Compiler_Main

  Main entry point for the Snow programming language compiler toolchain.

  This module provides the command-line interface for Snow compiler commands: -
  snow build: Compile source files to executable binaries - snow test: Run test
  files - snow fmt: Format source code - snow repl: Interactive read-eval-print
  loop - snow version: Display version information

  Follows traditional compiler design with standard option patterns: -
  Input/output file specification - Optimization levels (-O0, -O1, -O2, -O3) -
  Debug information generation (-g) - Warning controls (-W, -Werror) - Target
  architecture specification *)

open Error_reporting
open Build_pipeline

(** Helper function for string formatting *)
let _sprintf = Printf.sprintf

type compile_options = {
  input_files : string list; (* Source files to compile *)
  output_file : string option; (* Output executable name *)
  optimization_level : int; (* 0=none, 1=basic, 2=aggressive, 3=max *)
  debug_info : bool; (* Generate debug information *)
  warnings_as_errors : bool; (* Treat warnings as errors *)
  verbose : bool; (* Verbose output *)
  target_arch : string; (* Target architecture (x86_64, aarch64, etc.) *)
  link_libraries : string list; (* Additional libraries to link *)
  library_paths : string list; (* Library search paths *)
  include_paths : string list; (* Include search paths *)
}
(** Compilation options and flags *)

(** Default compilation options *)
let default_options =
  {
    input_files = [];
    output_file = None;
    optimization_level = 0;
    debug_info = false;
    warnings_as_errors = false;
    verbose = false;
    target_arch = "x86_64";
    link_libraries = [];
    library_paths = [];
    include_paths = [];
  }

exception Compiler_error of string

(** Display version information *)
let show_version () =
  Printf.printf "❄️  Snow Programming Language Compiler v0.1.0 ❄️\n\n";
  Printf.printf "🔧 Built with LLVM backend\n";
  Printf.printf "🎯 Target: %s\n" (Llvm_target.Target.default_triple ());
  Printf.printf "📋 License: MIT License\n\n";
  Printf.printf "🌟 Enjoying Snow? Consider sponsoring the development!\n";
  Printf.printf "💝 Support: https://github.com/sponsors/nao1215\n\n";
  Printf.printf "📚 Documentation: https://github.com/nao1215/snow-toolchain\n";
  Printf.printf
    "🐛 Report bugs: https://github.com/nao1215/snow-toolchain/issues\n"

(** Display help message *)
let show_help () =
  let help_text =
    {|
Snow Programming Language Compiler

USAGE:
    snow <SUBCOMMAND> [OPTIONS] <INPUT_FILES>

SUBCOMMANDS:
    build       Compile source files to executable binary
    run         Compile and run a Snow program immediately
    test        Run test files (*_test.sw)
    fmt         Format source code files
    mod init    Initialize a new module with snow.mod file
    mod tidy    Add missing dependencies and remove unused ones
    install     Download, compile and install remote packages with main package
    repl        Start interactive read-eval-print loop
    version     Show version information
    help        Show this help message

BUILD OPTIONS:
    -o <file>           Output executable name
    -O <level>          Optimization level (0-3)
    -g                  Generate debug information
    -Werror             Treat warnings as errors
    -v, --verbose       Verbose compilation output
    --target <arch>     Target architecture (x86_64, aarch64)
    -L <path>           Add library search path
    -l <library>        Link with library
    -I <path>           Add include search path

EXAMPLES:
    snow build main.sw                    # Compile to 'main' executable
    snow build -o hello src/main.sw       # Compile to 'hello' executable
    snow build -O2 -g main.sw             # Optimized build with debug info
    snow run main.sw                      # Compile and run immediately
    snow run main.sw -- arg1 arg2         # Run with arguments
    snow test                             # Run all test files in current directory
    snow fmt **/*.sw                      # Format all Snow files
    snow mod init                         # Initialize module in current directory
    snow mod init github.com/user/proj    # Initialize with specific module name
    snow mod tidy                         # Update dependencies
    snow install github.com/user/tool@v1.2.3  # Install specific version of tool
    snow repl                             # Start interactive session

For more information, see: https://github.com/nao1215/snow-toolchain

🌟 Enjoying Snow? Consider sponsoring the development!
💝 Support: https://github.com/sponsors/nao1215
|}
  in
  Printf.printf "%s\n" help_text

(** Parse command line arguments *)
let rec parse_args args options =
  match args with
  | [] -> options
  | "-o" :: output :: rest ->
      parse_args rest { options with output_file = Some output }
  | "-O0" :: rest -> parse_args rest { options with optimization_level = 0 }
  | "-O1" :: rest -> parse_args rest { options with optimization_level = 1 }
  | "-O2" :: rest -> parse_args rest { options with optimization_level = 2 }
  | "-O3" :: rest -> parse_args rest { options with optimization_level = 3 }
  | "-g" :: rest -> parse_args rest { options with debug_info = true }
  | "-Werror" :: rest ->
      parse_args rest { options with warnings_as_errors = true }
  | "-v" :: rest | "--verbose" :: rest ->
      parse_args rest { options with verbose = true }
  | "--target" :: arch :: rest ->
      parse_args rest { options with target_arch = arch }
  | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-L" ->
      let path = String.sub arg 2 (String.length arg - 2) in
      parse_args rest
        { options with library_paths = path :: options.library_paths }
  | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-l" ->
      let lib = String.sub arg 2 (String.length arg - 2) in
      parse_args rest
        { options with link_libraries = lib :: options.link_libraries }
  | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "-I" ->
      let path = String.sub arg 2 (String.length arg - 2) in
      parse_args rest
        { options with include_paths = path :: options.include_paths }
  | arg :: _ when String.length arg > 0 && String.get arg 0 = '-' ->
      raise (Compiler_error ("Unknown option: " ^ arg))
  | file :: rest ->
      parse_args rest { options with input_files = file :: options.input_files }

(** Main build command implementation *)
let build_command args =
  let options = parse_args args default_options in

  if List.length options.input_files = 0 then
    raise (Compiler_error "No input files specified");

  if options.verbose then (
    Printf.eprintf "Snow compiler starting...\n";
    Printf.eprintf "Input files: %s\n" (String.concat " " options.input_files));

  (* Determine output filename *)
  let output_name =
    match options.output_file with
    | Some name -> name
    | None ->
        (* Default: use first input file without extension *)
        let first_file = List.hd options.input_files in
        let basename =
          Filename.remove_extension (Filename.basename first_file)
        in
        basename
  in

  try
    (* Convert compile options to build options *)
    let build_options =
      {
        Build_pipeline.verbose = options.verbose;
        Build_pipeline.optimization_level = options.optimization_level;
        Build_pipeline.debug_info = options.debug_info;
        Build_pipeline.target_arch = options.target_arch;
      }
    in

    (* Process each input file using shared pipeline *)
    let object_files = ref [] in
    List.iter
      (fun input_file ->
        if options.verbose then Printf.eprintf "Processing: %s\n" input_file;

        (* Use shared build pipeline *)
        let build_result =
          Build_pipeline.build_program_pipeline input_file build_options
        in

        (* Generate object file *)
        let obj_file = Filename.remove_extension input_file ^ ".o" in
        Build_pipeline.generate_object_file build_result.llvm_module obj_file
          build_options;
        object_files := obj_file :: !object_files)
      options.input_files;

    (* Link all object files using shared pipeline *)
    Build_pipeline.link_executable !object_files output_name build_options
      options.library_paths options.link_libraries;

    (* Clean up object files unless verbose mode *)
    if not options.verbose then
      List.iter
        (fun obj_file -> try Sys.remove obj_file with _ -> ())
        !object_files;

    Printf.printf "Build successful: %s\n" output_name
  with
  | Build_pipeline_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error: Build failed: %s\n" (Printexc.to_string exn);
      exit 1

(** Format command implementation *)
let fmt_command args =
  let files =
    match args with
    | [] ->
        (* Format all .sw files in current directory *)
        let entries = Sys.readdir "." |> Array.to_list in
        List.filter (fun f -> Filename.check_suffix f ".sw") entries
    | _ -> args
  in

  if List.length files = 0 then
    Printf.printf "No Snow source files found to format\n"
  else
    match Formatter.format_files files with
    | Ok () -> ()
    | Error msg ->
        Printf.eprintf "Format error: %s\n" msg;
        exit 1

(** Main entry point *)
let main () =
  try
    match Sys.argv with
    | [| _ |] -> show_help ()
    | [| _; "version" |] -> show_version ()
    | [| _; "help" |] -> show_help ()
    | [| _; "--help" |] -> show_help ()
    | [| _; "-h" |] -> show_help ()
    | _ when Array.length Sys.argv >= 2 -> (
        match Sys.argv.(1) with
        | "build" ->
            let build_args =
              Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
            in
            build_command build_args
        | "run" ->
            let run_args =
              Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
            in
            Run.run_command run_args
        | "test" -> Printf.printf "Test command not yet implemented\n"
        | "fmt" ->
            let fmt_args =
              Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
            in
            fmt_command fmt_args
        | "mod" when Array.length Sys.argv >= 3 -> (
            match Sys.argv.(2) with
            | "init" ->
                let mod_args =
                  Array.to_list
                    (Array.sub Sys.argv 3 (Array.length Sys.argv - 3))
                in
                Mod_init.mod_init_command mod_args
            | "tidy" ->
                let mod_args =
                  Array.to_list
                    (Array.sub Sys.argv 3 (Array.length Sys.argv - 3))
                in
                Mod_tidy.mod_tidy_command mod_args
            | unknown_mod ->
                Printf.eprintf "Unknown mod subcommand: %s\n" unknown_mod;
                Printf.eprintf "Available: init, tidy\n";
                exit 1)
        | "install" ->
            let install_args =
              Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
            in
            Mod_install.install_command install_args
        | "repl" -> Printf.printf "REPL command not yet implemented\n"
        | unknown ->
            Printf.eprintf "Unknown subcommand: %s\n" unknown;
            Printf.eprintf "Run 'snow help' for usage information.\n";
            exit 1)
    | _ -> show_help ()
  with
  | Compiler_error _ ->
      (* Error already printed by enhanced error handling *)
      exit 1
  | exn ->
      let error_info =
        compiler_error ~code:"E999" ~title:"Internal compiler error"
          ~message:("Unexpected error: " ^ Printexc.to_string exn)
          ~suggestions:
            [
              "This is likely a bug in the SNOW compiler";
              "Please report this issue with details about what you were \
               trying to compile";
              "Try running with --verbose for more debugging information";
            ]
          ()
      in
      print_error error_info "";
      exit 1

(* Run main function *)
let () = main ()
