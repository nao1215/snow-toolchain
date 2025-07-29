(*** @project snow-toolchain @module Snow_Compiler_Package_Resolver

  Package resolution and fetching system for the Snow programming language.

  This module provides functionality to resolve and fetch external packages,
  manage package cache, and integrate with the build system to enable import
  statements like "github.com/user/repo/subpackage".

  Features: - Package resolution from import statements - Git-based package
  fetching - Package cache management ($SNOWPATH/pkg/mod/) - Import path to file
  path resolution - Integration with snow.mod dependency management *)

open Mod_parser

exception Package_error of string

type package_info = {
  import_path : string;
  repository_path : string; (* e.g., github.com/nao1215/snow *)
  subpackage : string; (* e.g., fmt *)
  version : string;
  cache_path : string;
}

(** Get SNOWPATH environment variable or default *)
let get_snow_path () =
  match Sys.getenv_opt "SNOWPATH" with
  | Some path -> path
  | None -> Filename.concat (Sys.getenv "HOME") "snow"

(** Get package cache directory *)
let get_package_cache_dir () = Filename.concat (get_snow_path ()) "pkg/mod"

(** Convert repository path to cache directory path *)
let repository_to_cache_path repo_path version =
  let cache_dir = get_package_cache_dir () in
  let versioned_path = Printf.sprintf "%s@%s" repo_path version in
  Filename.concat cache_dir versioned_path

(** Check if repository is already in cache *)
let is_repository_cached repo_path version =
  let cache_path = repository_to_cache_path repo_path version in
  Sys.file_exists cache_path

(** Parse import path to extract repository and subpackage *)
let parse_import_path import_path =
  (* Format: "github.com/user/repo/subpackage/..." *)
  let parts = String.split_on_char '/' import_path in
  match parts with
  | domain :: user :: repo :: rest when String.contains domain '.' ->
      (* Repository is domain/user/repo *)
      let repository_path = String.concat "/" [ domain; user; repo ] in
      let subpackage = String.concat "/" rest in
      (repository_path, subpackage)
  | _ -> raise (Package_error ("Invalid import path: " ^ import_path))

(** Execute command and return output *)
let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let output =
    try really_input_string ic (in_channel_length ic) with _ -> ""
  in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> output
  | _ -> raise (Package_error ("Command failed: " ^ cmd))

(** Get available versions (git tags) for a repository *)
let get_repository_versions repo_url =
  try
    let cmd = Printf.sprintf "git ls-remote --tags %s 2>/dev/null" repo_url in
    let output = run_command cmd in
    let lines = String.split_on_char '\n' output in
    let tags =
      List.filter_map
        (fun line ->
          if line = "" then None
          else
            let parts = String.split_on_char '\t' line in
            match parts with
            | [ _hash; ref_name ] ->
                (* Extract tag name from refs/tags/... *)
                if String.starts_with ~prefix:"refs/tags/" ref_name then
                  let tag =
                    String.sub ref_name 10 (String.length ref_name - 10)
                  in
                  (* Remove ^{} suffix if present *)
                  if String.ends_with ~suffix:"^{}" tag then None else Some tag
                else None
            | _ -> None)
        lines
    in
    List.sort String.compare tags
  with _ -> []
(* Return empty list if fetching fails, let caller handle it *)

(** Get latest commit hash from repository *)
let get_latest_commit_hash repo_url =
  try
    let cmd = Printf.sprintf "git ls-remote %s HEAD 2>/dev/null" repo_url in
    let output = run_command cmd in
    let lines = String.split_on_char '\n' output in
    match lines with
    | first_line :: _ when first_line <> "" -> (
        let parts = String.split_on_char '\t' first_line in
        match parts with hash :: _ -> String.trim hash | [] -> "main")
    | _ -> "main"
  with _ -> "main"

(** Clone a repository from git *)
let fetch_repository repo_path version =
  let cache_path = repository_to_cache_path repo_path version in

  (* Create cache directory structure if it doesn't exist *)
  let cache_dir = get_package_cache_dir () in
  let rec mkdir_p path =
    if not (Sys.file_exists path) then (
      mkdir_p (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  mkdir_p cache_dir;

  (* Construct repository URL *)
  let repo_url = Printf.sprintf "https://%s.git" repo_path in

  Printf.printf "snow: downloading %s@%s\n" repo_path version;

  (* Clone the repository at specific version *)
  let temp_dir = Filename.temp_dir "snow_fetch_" "" in
  let clone_cmd =
    Printf.sprintf "git clone --depth 1 --branch %s %s %s 2>/dev/null" version
      repo_url temp_dir
  in

  try
    let _ = run_command clone_cmd in

    (* Move to cache location *)
    let parent_dir = Filename.dirname cache_path in
    if not (Sys.file_exists parent_dir) then mkdir_p parent_dir;

    let mv_cmd = Printf.sprintf "mv %s %s" temp_dir cache_path in
    let _ = Sys.command mv_cmd in

    (* Remove .git directory to save space *)
    let rm_git_cmd = Printf.sprintf "rm -rf %s/.git" cache_path in
    let _ = Sys.command rm_git_cmd in

    Printf.printf "snow: %s@%s downloaded\n" repo_path version;
    cache_path
  with exn ->
    (* Clean up temp directory on failure *)
    let _ = Sys.command (Printf.sprintf "rm -rf %s" temp_dir) in
    raise exn

(** Resolve repository version from snow.mod *)
let resolve_repository_version repo_path snow_mod =
  (* Look for repository in require section *)
  let dep_opt =
    List.find_opt (fun dep -> dep.name = repo_path) snow_mod.require
  in
  match dep_opt with
  | Some dep -> (
      match dep.constraint_type with
      | Exact v -> v
      | GreaterEqual v -> (
          (* Try to get latest version >= v *)
          let repo_url = Printf.sprintf "https://%s.git" repo_path in
          let versions = get_repository_versions repo_url in
          (* Find latest version that satisfies constraint *)
          let satisfying = List.filter (fun ver -> ver >= v) versions in
          match List.rev satisfying with latest :: _ -> latest | [] -> v))
  | None -> (
      (* Not in dependencies, try to fetch latest *)
      let repo_url = Printf.sprintf "https://%s.git" repo_path in
      let versions = get_repository_versions repo_url in
      match List.rev versions with
      | latest :: _ -> latest
      | [] ->
          (* No tags found, get latest commit hash *)
          get_latest_commit_hash repo_url)

(** Ensure repository is fetched *)
let ensure_repository_fetched repo_path snow_mod =
  let version = resolve_repository_version repo_path snow_mod in

  if is_repository_cached repo_path version then
    repository_to_cache_path repo_path version
  else fetch_repository repo_path version

(** Check if import path matches current repository module name *)
let is_current_module_import import_path snow_mod =
  let repo_path, _subpackage = parse_import_path import_path in
  repo_path = snow_mod.module_name
  || String.starts_with ~prefix:(snow_mod.module_name ^ "/") import_path

(** Resolve import path for current repository (local files) *)
let resolve_local_import import_path snow_mod source_file =
  (* Printf.eprintf "DEBUG: Checking if %s is local import for module %s\n"
     import_path snow_mod.module_name; *)
  if not (is_current_module_import import_path snow_mod) then
    (* Printf.eprintf "DEBUG: Not a current module import\n"; *)
    None
  else
    (* Printf.eprintf "DEBUG: Is current module import, resolving locally\n"; *)
    let _repo_path, subpackage = parse_import_path import_path in
    let source_dir = Filename.dirname source_file in
    (* Printf.eprintf "DEBUG: Source dir: %s, subpackage: %s\n" source_dir
       subpackage; *)

    (* Find repository root by looking for snow.mod *)
    let rec find_repo_root dir =
      let snow_mod_path = Filename.concat dir "snow.mod" in
      if Sys.file_exists snow_mod_path then dir
      else
        let parent = Filename.dirname dir in
        if parent = dir then dir (* reached filesystem root *)
        else find_repo_root parent
    in
    let repo_root = find_repo_root source_dir in

    (* Try multiple search strategies for local subpackage *)
    let search_dirs =
      [
        (* 1. From repository root with exact subpackage path *)
        (if subpackage = "" then repo_root
         else Filename.concat repo_root subpackage);
        (* 2. Relative to current source file directory *)
        (if subpackage = "" then source_dir
         else Filename.concat source_dir subpackage);
        (* 3. From current working directory with subpackage path *)
        (if subpackage = "" then Sys.getcwd ()
         else Filename.concat (Sys.getcwd ()) subpackage);
        (* 4. Check if subpackage exists as a directory relative to source
           file *)
        (let rel_subpackage = Filename.basename subpackage in
         if rel_subpackage = "" then source_dir
         else Filename.concat source_dir rel_subpackage);
      ]
    in

    let package_name =
      if subpackage = "" then "main" else Filename.basename subpackage
    in

    let rec try_search_dirs dirs =
      match dirs with
      | [] -> None
      | local_package_dir :: rest -> (
          (* Printf.eprintf "DEBUG: Checking local package dir: %s\n"
             local_package_dir; *)
          let possible_files =
            [
              Filename.concat local_package_dir (package_name ^ ".sw");
              Filename.concat local_package_dir "lib.sw";
              Filename.concat local_package_dir "mod.sw";
              Filename.concat local_package_dir "index.sw";
            ]
          in
          (* Printf.eprintf "DEBUG: Checking files: %s\n" (String.concat ", "
             possible_files); *)
          match List.find_opt Sys.file_exists possible_files with
          | Some file -> Some file
          | None -> try_search_dirs rest)
    in

    let found_file = try_search_dirs search_dirs in
    (* (match found_file with | Some file -> Printf.eprintf "DEBUG: Found file:
       %s\n" file | None -> Printf.eprintf "DEBUG: No files found\n"); *)
    found_file

(** Resolve import path to actual file path *)
let resolve_import_to_file import_path snow_mod source_file =
  (* Debug output (temporarily disabled) *)
  (* Printf.eprintf "DEBUG: Resolving import %s with module %s\n" import_path snow_mod.module_name;
  Printf.eprintf "DEBUG: Full snow_mod: module=%s snow_version=%s\n" snow_mod.module_name snow_mod.snow_version; *)

  (* First try to resolve as local import *)
  match resolve_local_import import_path snow_mod source_file with
  | Some local_file ->
      (* Printf.eprintf "DEBUG: Found local file: %s\n" local_file; *)
      local_file
  | None -> (
      (* Printf.eprintf "DEBUG: No local file found, trying remote resolution\n"; *)
      (* Fall back to remote package resolution *)
      let repo_path, subpackage = parse_import_path import_path in
      let cache_path = ensure_repository_fetched repo_path snow_mod in

      (* Construct full path to subpackage *)
      let package_dir =
        if subpackage = "" then cache_path
        else Filename.concat cache_path subpackage
      in

      (* Look for package files in order of preference *)
      let package_name =
        if subpackage = "" then "main" else Filename.basename subpackage
      in

      let possible_files =
        [
          Filename.concat package_dir (package_name ^ ".sw");
          Filename.concat package_dir "lib.sw";
          Filename.concat package_dir "mod.sw";
          Filename.concat package_dir "index.sw";
        ]
      in

      match List.find_opt Sys.file_exists possible_files with
      | Some file -> file
      | None ->
          (* List available files for debugging *)
          let files =
            try Sys.readdir package_dir |> Array.to_list with _ -> []
          in
          let sw_files =
            List.filter (fun f -> Filename.check_suffix f ".sw") files
          in
          raise
            (Package_error
               (Printf.sprintf
                  "No source file found for package %s in %s. Available .sw \
                   files: %s"
                  import_path package_dir
                  (String.concat ", " sw_files))))

(** Initialize package cache *)
let init_package_cache () =
  let cache_dir = get_package_cache_dir () in
  if not (Sys.file_exists cache_dir) then (
    Printf.printf "Initializing package cache at %s\n" cache_dir;
    let rec mkdir_p path =
      if not (Sys.file_exists path) then (
        mkdir_p (Filename.dirname path);
        Unix.mkdir path 0o755)
    in
    mkdir_p cache_dir)

(** Download and cache all dependencies from snow.mod *)
let download_dependencies snow_mod =
  init_package_cache ();

  List.iter
    (fun dep ->
      let version =
        match dep.constraint_type with
        | Exact v -> v
        | GreaterEqual _v ->
            (* Resolve to actual version *)
            resolve_repository_version dep.name snow_mod
      in

      if not (is_repository_cached dep.name version) then
        try
          let _ = fetch_repository dep.name version in
          ()
        with
        | Package_error msg ->
            Printf.eprintf "Warning: Failed to fetch %s@%s: %s\n" dep.name
              version msg
        | exn ->
            Printf.eprintf "Warning: Failed to fetch %s@%s: %s\n" dep.name
              version (Printexc.to_string exn))
    snow_mod.require

(** Standard library packages that don't need fetching *)
let is_stdlib_package import_path =
  List.exists
    (fun prefix -> String.starts_with ~prefix import_path)
    [ "std/"; "builtin/"; "runtime/"; "core/" ]

(** Local import (relative path) *)
let is_local_import import_path =
  String.starts_with ~prefix:"./" import_path
  || String.starts_with ~prefix:"../" import_path

(** Extract imports from a source file *)
let extract_imports_from_file source_file =
  let imports = ref [] in

  try
    let ic = open_in source_file in
    (try
       while true do
         let line = input_line ic in
         let trimmed = String.trim line in
         (* Parse import statements *)
         if String.starts_with ~prefix:"import \"" trimmed then
           let start_quote = 8 in
           let end_quote = String.rindex trimmed '"' in
           if end_quote > start_quote then
             let import_path =
               String.sub trimmed start_quote (end_quote - start_quote)
             in
             imports := import_path :: !imports
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !imports
  with Sys_error msg ->
    Printf.eprintf "Warning: Could not read file %s: %s\n" source_file msg;
    []

(** Resolve all imports in a source file *)
let resolve_file_imports source_file snow_mod =
  let imports = extract_imports_from_file source_file in

  (* Resolve each import *)
  List.filter_map
    (fun import_path ->
      if is_stdlib_package import_path then None (* Skip stdlib for now *)
      else if is_local_import import_path then None
        (* Skip local imports for now *)
      else
        try
          let file_path =
            resolve_import_to_file import_path snow_mod source_file
          in
          Some (import_path, file_path)
        with
        | Package_error msg ->
            Printf.eprintf "Error resolving import %s: %s\n" import_path msg;
            None
        | exn ->
            Printf.eprintf "Error resolving import %s: %s\n" import_path
              (Printexc.to_string exn);
            None)
    imports

(** Get package info for an import *)
let get_package_info import_path snow_mod =
  let repo_path, subpackage = parse_import_path import_path in
  let version = resolve_repository_version repo_path snow_mod in
  let cache_path = repository_to_cache_path repo_path version in
  { import_path; repository_path = repo_path; subpackage; version; cache_path }
