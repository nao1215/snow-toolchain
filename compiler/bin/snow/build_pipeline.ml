(*** @project snow-toolchain @module Snow_Compiler_Build_Pipeline

  Shared build pipeline for the Snow programming language compiler.

  This module provides common build functionality used by both 'snow build' and
  'snow run' commands to ensure consistent behavior and reduce code duplication.

  Features: - Common source file parsing and package linking - LLVM code
  generation - Object file generation - Error handling standardization *)

open Ast
open Codegen
open Package_linker

exception Build_pipeline_error of string

type build_options = {
  verbose : bool;
  optimization_level : int;
  debug_info : bool;
  target_arch : string;
}

type build_result = {
  linked_program : program;
  package_context : package_context;
  llvm_module : Llvm.llmodule;
}

(** Default build options *)
let default_build_options =
  {
    verbose = false;
    optimization_level = 0;
    debug_info = false;
    target_arch = "x86_64";
  }

(** Load and parse snow.mod file or create default *)
let load_snow_mod_or_default verbose =
  try
    if Mod_parser.snow_mod_exists () then Mod_parser.load_current_snow_mod ()
    else (
      if verbose then
        Printf.eprintf "No snow.mod found, using default configuration\n";
      {
        module_name = "main";
        snow_version = "0.1.0";
        require = [];
        replace = [];
        exclude = [];
        retract = [];
      })
  with Mod_parser.Mod_parse_error msg ->
    if verbose then Printf.eprintf "Warning: Could not load snow.mod: %s\n" msg;
    {
      module_name = "main";
      snow_version = "0.1.0";
      require = [];
      replace = [];
      exclude = [];
      retract = [];
    }

(** Parse source file and return AST *)
let parse_source_file filename verbose =
  if verbose then Printf.eprintf "Parsing: %s\n" filename;

  try
    let content =
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in

    let tokens = Lexer.lex content in
    match Parser.parse_program_with_error_handling tokens with
    | Ok program -> program
    | Error msg ->
        raise (Build_pipeline_error ("Parse error in " ^ filename ^ ": " ^ msg))
  with
  | Sys_error msg -> raise (Build_pipeline_error ("File error: " ^ msg))
  | Parser.Parse_error msg ->
      raise (Build_pipeline_error ("Parse error in " ^ filename ^ ": " ^ msg))
  | exn ->
      raise
        (Build_pipeline_error
           ("Unexpected error parsing " ^ filename ^ ": "
          ^ Printexc.to_string exn))

(** Link packages with program and handle errors consistently *)
let link_packages_safe program filename snow_mod verbose =
  try
    if verbose then Printf.eprintf "Linking packages for: %s\n" filename;
    let linked_program, package_context =
      link_packages_with_program program filename snow_mod
    in
    (linked_program, package_context)
  with
  | Package_link_error msg ->
      raise (Build_pipeline_error ("Package linking error: " ^ msg))
  | exn ->
      raise
        (Build_pipeline_error
           ("Unexpected package linking error: " ^ Printexc.to_string exn))

(** Generate LLVM IR from linked program *)
let generate_llvm_ir linked_program verbose =
  if verbose then Printf.eprintf "Generating LLVM IR...\n";

  try Codegen.codegen_program_with_package linked_program with
  | Codegen_error msg ->
      raise (Build_pipeline_error ("Code generation error: " ^ msg))
  | exn ->
      raise
        (Build_pipeline_error
           ("LLVM IR generation failed: " ^ Printexc.to_string exn))

(** Common build pipeline: parse, link, and generate IR *)
let build_program_pipeline filename options =
  (* Load snow.mod configuration *)
  let snow_mod = load_snow_mod_or_default options.verbose in

  (* Parse source file *)
  let program = parse_source_file filename options.verbose in

  (* Link packages with program *)
  let linked_program, package_context =
    link_packages_safe program filename snow_mod options.verbose
  in

  (* Generate LLVM IR *)
  let llvm_module = generate_llvm_ir linked_program options.verbose in

  { linked_program; package_context; llvm_module }

(** Generate object file from LLVM module *)
let generate_object_file llvm_module output_path options =
  if options.verbose then
    Printf.eprintf "Generating object file: %s\n" output_path;

  try
    (* Initialize LLVM target *)
    Llvm_all_backends.initialize ();
    let target_triple =
      match options.target_arch with
      | "x86_64" -> "x86_64-unknown-linux-gnu"
      | "aarch64" -> "aarch64-unknown-linux-gnu"
      | arch -> arch ^ "-unknown-linux-gnu"
    in

    (* Set target triple *)
    Llvm.set_target_triple target_triple llvm_module;

    (* Get target machine *)
    let target = Llvm_target.Target.by_triple target_triple in
    let machine =
      Llvm_target.TargetMachine.create ~triple:target_triple ~cpu:""
        ~features:""
        ~level:
          (match options.optimization_level with
          | 0 -> Llvm_target.CodeGenOptLevel.None
          | 1 -> Llvm_target.CodeGenOptLevel.Less
          | 2 -> Llvm_target.CodeGenOptLevel.Default
          | _ -> Llvm_target.CodeGenOptLevel.Aggressive)
        ~reloc_mode:Llvm_target.RelocMode.PIC
        ~code_model:Llvm_target.CodeModel.Default target
    in

    (* Write object file *)
    Llvm_target.TargetMachine.emit_to_file llvm_module
      Llvm_target.CodeGenFileType.ObjectFile output_path machine;

    if options.verbose then
      Printf.eprintf "Object file generated: %s\n" output_path
  with exn ->
    raise
      (Build_pipeline_error
         ("Object file generation failed: " ^ Printexc.to_string exn))

(** Link object files to executable *)
let link_executable object_files output_path options library_paths
    link_libraries =
  if options.verbose then Printf.eprintf "Linking executable: %s\n" output_path;

  let linker_cmd = Buffer.create 256 in
  Buffer.add_string linker_cmd "gcc";

  (* Add object files *)
  List.iter (fun obj -> Buffer.add_string linker_cmd (" " ^ obj)) object_files;

  (* Output file *)
  Buffer.add_string linker_cmd (" -o " ^ output_path);

  (* Library paths *)
  List.iter
    (fun path -> Buffer.add_string linker_cmd (" -L" ^ path))
    library_paths;

  (* Libraries *)
  List.iter
    (fun lib -> Buffer.add_string linker_cmd (" -l" ^ lib))
    link_libraries;

  (* Standard runtime libraries *)
  Buffer.add_string linker_cmd " -lm -lpthread";

  let cmd = Buffer.contents linker_cmd in
  if options.verbose then Printf.eprintf "Linker command: %s\n" cmd;

  let result = Sys.command cmd in
  if result <> 0 then
    raise
      (Build_pipeline_error
         ("Linking failed with exit code: " ^ string_of_int result));

  if options.verbose then Printf.eprintf "Executable created: %s\n" output_path

(** Complete build pipeline: source to executable *)
let build_to_executable filename output_path options library_paths
    link_libraries =
  (* Build program *)
  let build_result = build_program_pipeline filename options in

  (* Generate object file *)
  let temp_object = Filename.temp_file "snow_build" ".o" in
  generate_object_file build_result.llvm_module temp_object options;

  (* Link to executable *)
  link_executable [ temp_object ] output_path options library_paths
    link_libraries;

  (* Clean up temporary object file *)
  (try Sys.remove temp_object with _ -> ());

  build_result
