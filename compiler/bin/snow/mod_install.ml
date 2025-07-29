(*** @project snow-toolchain @module Snow_Compiler_Mod_Install

  Package installation for the Snow programming language.

  This module provides the 'snow install' subcommand that downloads, compiles,
  and installs remote Snow packages containing main packages, similar to 'go
  install'. It enables users to easily install Snow-based command-line tools and
  executables from remote repositories.

  Features: - Download packages from Git repositories (GitHub, GitLab, etc.) -
  Cache downloaded source code in $SNOWPATH/pkg/mod/ - Build main packages and
  install executables to $SNOWPATH/bin/ - Version resolution (tags, branches,
  commits) - Force reinstallation and cache management - Integration with
  snow.mod for binary names and dependencies *)

open Mod_parser

exception Install_error of string

type version_spec =
  | Latest
  | Tag of string
  | Branch of string
  | Commit of string

type install_result = {
  module_path : string;
  version : string;
  executable_name : string;
  install_path : string;
  cached : bool;
}

(** Get SNOWPATH directory, defaults to $HOME/snow *)
let get_snow_path () =
  try Sys.getenv "SNOWPATH"
  with Not_found ->
    let home = try Sys.getenv "HOME" with Not_found -> "." in
    Filename.concat home "snow"

(** Get SNOWBIN directory, defaults to $SNOWPATH/bin *)
let get_snow_bin () =
  try Sys.getenv "SNOWBIN"
  with Not_found -> Filename.concat (get_snow_path ()) "bin"

(** Get module cache directory *)
let get_module_cache_dir () = Filename.concat (get_snow_path ()) "pkg/mod"

(** Create directory recursively *)
let rec create_directory dir =
  if not (Sys.file_exists dir) then (
    create_directory (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

(** Ensure required directories exist *)
let ensure_directories () =
  create_directory (get_snow_path ());
  create_directory (get_module_cache_dir ());
  create_directory (get_snow_bin ())

(** Parse module path with optional version *)
let parse_module_spec module_spec =
  if String.contains module_spec '@' then
    let parts = String.split_on_char '@' module_spec in
    match parts with
    | [ module_path; version_str ] ->
        let version =
          if version_str = "latest" then Latest
          else if String.starts_with ~prefix:"v" version_str then
            Tag version_str
          else if String.length version_str >= 7 then Commit version_str
          else Branch version_str
        in
        (module_path, version)
    | _ ->
        raise (Install_error ("Invalid module specification: " ^ module_spec))
  else (module_spec, Latest)

(** Split module path into repository path and subpath *)
let split_module_path module_path =
  let parts = String.split_on_char '/' module_path in
  match parts with
  | [] | [ _ ] | [ _; _ ] -> (module_path, None) (* No subpath *)
  | host :: user :: repo :: subpath_parts ->
      let repo_path = String.concat "/" [ host; user; repo ] in
      let subpath = String.concat "/" subpath_parts in
      (repo_path, Some subpath)

(** Convert repository path to git URL *)
let repo_path_to_git_url repo_path =
  if String.starts_with ~prefix:"github.com/" repo_path then
    "https://" ^ repo_path ^ ".git"
  else if String.starts_with ~prefix:"gitlab.com/" repo_path then
    "https://" ^ repo_path ^ ".git"
  else raise (Install_error ("Unsupported git hosting: " ^ repo_path))

(** Get cache directory for specific module version *)
let get_module_version_dir module_path version_str =
  let cache_dir = get_module_cache_dir () in
  let safe_module_path =
    String.map (function '/' -> '/' | c -> c) module_path
  in
  Filename.concat cache_dir (safe_module_path ^ "@" ^ version_str)

(** Resolve latest git tag *)
let resolve_latest_tag git_url verbose =
  if verbose then Printf.eprintf "Resolving latest tag for %s\n" git_url;

  let cmd =
    Printf.sprintf
      "git ls-remote --tags --sort=-version:refname %s | head -n1 | cut -f2 | \
       sed 's|refs/tags/||'"
      git_url
  in
  let ic = Unix.open_process_in cmd in
  let result =
    try
      let line = input_line ic in
      String.trim line
    with End_of_file -> "main"
  in
  let _ = Unix.close_process_in ic in
  if result = "" then "main" else result

(** Get version string for cache directory *)
let get_version_string module_path version verbose =
  match version with
  | Latest ->
      let repo_path, _ = split_module_path module_path in
      let git_url = repo_path_to_git_url repo_path in
      resolve_latest_tag git_url verbose
  | Tag tag -> tag
  | Branch branch -> branch
  | Commit commit -> commit

(** Check if module is already cached *)
let is_cached module_path version_str =
  let cache_dir = get_module_version_dir module_path version_str in
  Sys.file_exists cache_dir && Sys.is_directory cache_dir

(** Download module to cache *)
let download_module module_path version_str verbose =
  let repo_path, _ = split_module_path module_path in
  let git_url = repo_path_to_git_url repo_path in
  let cache_dir = get_module_version_dir module_path version_str in

  if verbose then
    Printf.eprintf "Downloading %s@%s to %s\n" module_path version_str cache_dir;

  (* Create parent directories *)
  create_directory (Filename.dirname cache_dir);

  (* Clone repository *)
  let clone_cmd =
    if String.starts_with ~prefix:"v" version_str then
      Printf.sprintf "git clone --depth 1 --branch %s %s %s" version_str git_url
        cache_dir
    else Printf.sprintf "git clone --depth 1 %s %s" git_url cache_dir
  in

  if verbose then Printf.eprintf "Running: %s\n" clone_cmd;

  let result = Sys.command clone_cmd in
  if result <> 0 then
    raise
      (Install_error
         (Printf.sprintf "Failed to download %s@%s" module_path version_str));

  (* Remove .git directory to save space *)
  let git_dir = Filename.concat cache_dir ".git" in
  (if Sys.file_exists git_dir then
     let rm_cmd = Printf.sprintf "rm -rf %s" (Filename.quote git_dir) in
     ignore (Sys.command rm_cmd));

  cache_dir

(** Find main package file in module *)
let find_main_package module_path cache_dir =
  (* Printf.eprintf "DEBUG: Finding main package for %s in %s\n" module_path
     cache_dir; *)
  let _repo_path, subpath = split_module_path module_path in

  (* If there's a subpath, look in that directory first *)
  let subpath_candidates =
    match subpath with
    | Some sub ->
        [
          (* Look in the subpath directory within the cache *)
          Filename.concat cache_dir (sub ^ "/main.sw");
          Filename.concat cache_dir (sub ^ "/src/main.sw");
        ]
    | None -> []
  in

  (* Standard main file locations *)
  let standard_candidates =
    [
      Filename.concat cache_dir "main.sw";
      Filename.concat cache_dir "cmd/main.sw";
      Filename.concat cache_dir "src/main.sw";
    ]
  in

  let all_candidates = subpath_candidates @ standard_candidates in

  let rec find_main files =
    match files with
    | [] -> None
    | file :: rest -> if Sys.file_exists file then Some file else find_main rest
  in

  let result = find_main all_candidates in

  (* If no main file found with standard patterns and we have a subpath, try to
     recursively search in the cache directory *)
  match (result, subpath) with
  | None, Some _sub ->
      let rec search_directory dir =
        try
          let entries = Sys.readdir dir |> Array.to_list in
          let main_files = List.filter (fun f -> f = "main.sw") entries in
          if main_files <> [] then Some (Filename.concat dir "main.sw")
          else
            let subdirs =
              List.filter
                (fun f ->
                  let path = Filename.concat dir f in
                  Sys.file_exists path && Sys.is_directory path)
                entries
            in
            List.fold_left
              (fun acc subdir ->
                match acc with
                | Some _file -> acc
                | None -> search_directory (Filename.concat dir subdir))
              None subdirs
        with _ -> None
      in
      search_directory cache_dir
  | _ -> result

(** Extract binary name from snow.mod or module path *)
let get_binary_name module_path cache_dir =
  let repo_path, subpath = split_module_path module_path in
  let snow_mod_file = Filename.concat cache_dir "snow.mod" in
  if Sys.file_exists snow_mod_file then
    try
      let _mod_info = parse_snow_mod snow_mod_file in
      (* TODO: Add bin_name support to module_metadata when needed *)
      (* Use subpath as binary name if available, otherwise use repository name *)
      match subpath with
      | Some sub -> Filename.basename sub
      | None -> Filename.basename repo_path
    with _ -> (
      match subpath with
      | Some sub -> Filename.basename sub
      | None -> Filename.basename repo_path)
  else
    match subpath with
    | Some sub -> Filename.basename sub
    | None -> Filename.basename repo_path

(** Compile main package to executable *)
let compile_main_package main_file output_path verbose =
  if verbose then Printf.eprintf "Compiling %s to %s\n" main_file output_path;

  (* Change to the directory containing the main file to ensure proper snow.mod
     resolution *)
  let original_dir = Sys.getcwd () in
  let main_dir = Filename.dirname main_file in

  (* Find the repository root by looking for snow.mod *)
  let rec find_repo_root dir =
    let snow_mod_path = Filename.concat dir "snow.mod" in
    if Sys.file_exists snow_mod_path then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then dir
        (* reached filesystem root, use main_dir as fallback *)
      else find_repo_root parent
  in
  let repo_root = find_repo_root main_dir in

  if verbose then Printf.eprintf "Changing to repository root: %s\n" repo_root;

  try
    Sys.chdir repo_root;

    (* Calculate relative path from repo root to main file *)
    let relative_main_path =
      if String.starts_with ~prefix:(repo_root ^ "/") main_file then
        String.sub main_file
          (String.length repo_root + 1)
          (String.length main_file - String.length repo_root - 1)
      else main_file
    in

    (* Use snow binary directly - find it in PATH or use the current executable
       location *)
    let snow_binary =
      try
        let which_result = Unix.open_process_in "which snow" in
        let path = String.trim (input_line which_result) in
        let _ = Unix.close_process_in which_result in
        path
      with _ ->
        (* Fallback: use the current executable path if 'which' fails *)
        Sys.executable_name
    in

    let compile_cmd =
      Printf.sprintf "%s build -o %s %s"
        (Filename.quote snow_binary)
        (Filename.quote output_path)
        (Filename.quote relative_main_path)
    in

    if verbose then Printf.eprintf "Running: %s\n" compile_cmd;

    let result = Sys.command compile_cmd in

    (* Restore original directory *)
    Sys.chdir original_dir;

    if result <> 0 then
      raise (Install_error ("Compilation failed for " ^ main_file))
  with exn ->
    (* Ensure we restore the original directory even on exception *)
    Sys.chdir original_dir;
    raise exn

(** Install module *)
let install_module module_path version force verbose =
  let version_str = get_version_string module_path version verbose in
  let cache_dir = get_module_version_dir module_path version_str in

  let cached = is_cached module_path version_str in

  (* Download if not cached or force reinstall *)
  let final_cache_dir =
    if (not cached) || force then (
      if force && cached then (
        if verbose then
          Printf.eprintf "Force reinstall: removing cached version\n";
        let rm_cmd = Printf.sprintf "rm -rf %s" (Filename.quote cache_dir) in
        ignore (Sys.command rm_cmd));
      download_module module_path version_str verbose)
    else (
      if verbose then Printf.eprintf "Using cached version at %s\n" cache_dir;
      cache_dir)
  in

  (* Find main package *)
  let main_file =
    match find_main_package module_path final_cache_dir with
    | Some file -> file
    | None -> raise (Install_error ("No main package found in " ^ module_path))
  in

  (* Get binary name *)
  let binary_name = get_binary_name module_path final_cache_dir in
  let install_path = Filename.concat (get_snow_bin ()) binary_name in

  (* Compile and install *)
  compile_main_package main_file install_path verbose;

  if verbose then Printf.eprintf "Installed %s to %s\n" binary_name install_path;

  {
    module_path;
    version = version_str;
    executable_name = binary_name;
    install_path;
    cached;
  }

(** Display help message for install command *)
let show_install_help () =
  let help_text =
    {|
Snow Install Command

USAGE:
    snow install [OPTIONS] <module-path>[@version]

DESCRIPTION:
    Download, compile, and install remote Snow packages that contain a main package.
    Similar to 'go install', this enables users to easily install Snow-based 
    command-line tools and executables from remote repositories.

ARGUMENTS:
    <module-path>[@version]    Module path with optional version specification
                              Examples:
                                github.com/user/tool
                                github.com/user/tool@v1.2.3
                                github.com/user/tool@latest
                                github.com/user/tool@main
                                github.com/user/tool@commit-hash

OPTIONS:
    -f, --force       Force reinstallation even if already installed
    -v, --verbose     Verbose output showing download and build progress
    --no-cache        Skip cache and always download fresh copy
    -h, --help        Show this help message

ENVIRONMENT:
    SNOWPATH          Snow workspace directory (defaults to $HOME/snow)
    SNOWBIN           Directory for installed executables (defaults to $SNOWPATH/bin)

EXAMPLES:
    snow install github.com/user/snow-formatter
    snow install github.com/user/snow-lint@v2.1.0
    snow install github.com/user/experimental-tool@develop
    snow install -f -v github.com/user/tool@v1.0.0

NOTES:
    - Executables are installed to $SNOWPATH/bin
    - Add $SNOWPATH/bin to your PATH to use installed binaries
    - Source code is cached in $SNOWPATH/pkg/mod for reuse

For more information: https://github.com/nao1215/snow-toolchain
|}
  in
  Printf.printf "%s\n" help_text

(** Main install command implementation *)
let install_command args =
  let rec parse_args args force verbose no_cache modules =
    match args with
    | [] -> (force, verbose, no_cache, List.rev modules)
    | "-f" :: rest | "--force" :: rest ->
        parse_args rest true verbose no_cache modules
    | "-v" :: rest | "--verbose" :: rest ->
        parse_args rest force true no_cache modules
    | "--no-cache" :: rest -> parse_args rest force verbose true modules
    | "-h" :: _ | "--help" :: _ ->
        show_install_help ();
        exit 0
    | module_spec :: rest ->
        parse_args rest force verbose no_cache (module_spec :: modules)
  in

  try
    let force, verbose, no_cache, modules =
      parse_args args false false false []
    in

    if List.length modules = 0 then
      raise
        (Install_error
           "No module specified. Usage: snow install <module-path>[@version]");

    if verbose then Printf.eprintf "Running snow install...\n";

    (* Ensure directories exist *)
    ensure_directories ();

    (* Install each module *)
    List.iter
      (fun module_spec ->
        let module_path, version = parse_module_spec module_spec in

        if verbose then Printf.eprintf "Installing %s\n" module_spec;

        let result =
          install_module module_path version (force || no_cache) verbose
        in

        Printf.printf "Installed %s@%s -> %s\n" result.module_path
          result.version result.executable_name;

        if
          not
            (List.mem (get_snow_bin ())
               (String.split_on_char ':'
                  (try Sys.getenv "PATH" with Not_found -> "")))
        then
          Printf.printf "Note: Add %s to your PATH to use installed binaries\n"
            (get_snow_bin ()))
      modules;

    Printf.printf "Installation completed successfully\n"
  with
  | Install_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      exit 1
