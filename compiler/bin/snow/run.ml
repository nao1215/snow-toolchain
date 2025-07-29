(*** @project snow-toolchain @module Snow_Compiler_Run

  Run command implementation for the Snow programming language compiler.

  This module provides the 'snow run' subcommand that compiles and immediately
  executes a Snow source file, similar to 'go run' in Go.

  Features: - Compile source file to temporary executable - Execute the compiled
  binary with arguments - Clean up temporary files after execution - Pass
  through command-line arguments to the program *)

open Build_pipeline

exception Run_error of string

type run_options = {
  source_file : string;
  program_args : string list;
  verbose : bool;
  optimization_level : int;
  keep_temp : bool; (* Keep temporary files for debugging *)
}

let default_run_options =
  {
    source_file = "";
    program_args = [];
    verbose = false;
    optimization_level = 0;
    keep_temp = false;
  }

(** Parse run command arguments *)
let parse_run_args args =
  let rec parse acc_options acc_args found_file =
    match acc_args with
    | [] ->
        if acc_options.source_file = "" then
          raise (Run_error "No source file specified")
        else acc_options
    | "--" :: rest ->
        (* Everything after -- is passed to the program *)
        { acc_options with program_args = acc_options.program_args @ rest }
    | "-v" :: rest | "--verbose" :: rest ->
        parse { acc_options with verbose = true } rest found_file
    | "-O0" :: rest ->
        parse { acc_options with optimization_level = 0 } rest found_file
    | "-O1" :: rest ->
        parse { acc_options with optimization_level = 1 } rest found_file
    | "-O2" :: rest ->
        parse { acc_options with optimization_level = 2 } rest found_file
    | "-O3" :: rest ->
        parse { acc_options with optimization_level = 3 } rest found_file
    | "--keep-temp" :: rest ->
        parse { acc_options with keep_temp = true } rest found_file
    | arg :: _ when String.length arg > 0 && String.get arg 0 = '-' ->
        raise (Run_error ("Unknown option: " ^ arg))
    | file :: rest when not found_file ->
        (* First non-option argument is the source file *)
        parse { acc_options with source_file = file } rest true
    | arg :: rest ->
        (* Additional arguments are passed to the program *)
        parse
          { acc_options with program_args = acc_options.program_args @ [ arg ] }
          rest found_file
  in
  parse default_run_options args false

(** Execute the compiled program *)
let execute_program executable args verbose =
  if verbose then (
    Printf.eprintf "Executing: %s" executable;
    List.iter (Printf.eprintf " %s") args;
    Printf.eprintf "\n");

  (* Build command with arguments *)
  let cmd =
    if List.length args > 0 then
      Printf.sprintf "%s %s" executable (String.concat " " args)
    else executable
  in

  (* Execute and return exit code *)
  Sys.command cmd

(** Generate temporary file name *)
let temp_file_name base_name suffix =
  let temp_dir = Filename.get_temp_dir_name () in
  let pid = Unix.getpid () in
  let timestamp = Unix.time () |> int_of_float in
  Printf.sprintf "%s/snow_%s_%d_%d%s" temp_dir base_name pid timestamp suffix

(** Main run command implementation *)
let run_command args =
  let options = parse_run_args args in

  if options.verbose then (
    Printf.eprintf "Snow run: %s\n" options.source_file;
    if List.length options.program_args > 0 then
      Printf.eprintf "Program arguments: %s\n"
        (String.concat " " options.program_args));

  (* Generate temporary file names *)
  let base_name =
    Filename.remove_extension (Filename.basename options.source_file)
  in
  let temp_object = temp_file_name base_name ".o" in
  let temp_executable = temp_file_name base_name "" in

  let cleanup () =
    if not options.keep_temp then (
      (try Sys.remove temp_object with _ -> ());
      try Sys.remove temp_executable with _ -> ())
  in

  try
    (* Convert run options to build options *)
    let build_options =
      {
        Build_pipeline.verbose = options.verbose;
        Build_pipeline.optimization_level = options.optimization_level;
        Build_pipeline.debug_info = false;
        (* Run doesn't typically need debug info *)
        Build_pipeline.target_arch = "x86_64";
        (* Default for run *)
      }
    in

    (* Use shared build pipeline to create executable *)
    let _ =
      Build_pipeline.build_to_executable options.source_file temp_executable
        build_options [] []
    in

    (* Execute the program *)
    let exit_code =
      execute_program temp_executable options.program_args options.verbose
    in

    (* Clean up temporary files *)
    cleanup ();

    (* Exit with the same code as the executed program *)
    exit exit_code
  with
  | Build_pipeline_error msg ->
      cleanup ();
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Run_error msg ->
      cleanup ();
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      cleanup ();
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      exit 1
