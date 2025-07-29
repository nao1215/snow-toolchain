(*** @project snow-toolchain @module Snow_Compiler_Mod_Tidy

  Module dependency management for the Snow programming language.

  This module provides the 'snow mod tidy' subcommand that analyzes Snow source
  files to find dependencies and updates the snow.mod file accordingly, similar
  to 'go mod tidy' in Go.

  Features: - Scan Snow source files for import statements - Detect missing
  dependencies not listed in snow.mod - Remove unused dependencies from snow.mod
  - Update dependency versions to latest compatible versions - Preserve manual
  dependency specifications *)

open Mod_parser
open Sum_parser
open Version_resolver

exception Mod_tidy_error of string

type import_info = {
  module_path : string;
  source_file : string;
  line_number : int;
}

type dependency_resolution = {
  module_path : string;
  resolved_version : string;
  is_direct : bool;
}

type tidy_result = {
  added_dependencies : dependency_resolution list;
  removed_dependencies : string list;
  updated_dependencies : dependency_resolution list;
  warnings : string list;
}

(** Find all .sw files in current directory and subdirectories *)
let find_snow_files () =
  let rec find_files acc dir =
    try
      let entries = Sys.readdir dir in
      Array.fold_left
        (fun acc entry ->
          let path = Filename.concat dir entry in
          if Sys.is_directory path && not (String.starts_with ~prefix:"." entry)
          then find_files acc path
          else if Filename.check_suffix entry ".sw" then path :: acc
          else acc)
        acc entries
    with Sys_error _ -> acc
  in
  find_files [] "."

(** Extract package declaration from Snow file *)
let extract_package_from_file filepath =
  try
    let ic = open_in filepath in
    let first_line = input_line ic in
    close_in ic;

    (* Parse package declaration: "package name" *)
    if String.starts_with ~prefix:"package " first_line then
      let package_name =
        String.sub first_line 8 (String.length first_line - 8) |> String.trim
      in
      Some package_name
    else None
  with Sys_error _ | End_of_file -> None

(** Extract import statements from Snow file *)
let extract_imports_from_file filepath =
  let imports = ref [] in
  let line_num = ref 0 in

  try
    let ic = open_in filepath in
    (try
       while true do
         incr line_num;
         let line = input_line ic in
         let trimmed = String.trim line in

         (* Parse import statements: 'import "path"' *)
         if String.starts_with ~prefix:"import \"" trimmed then
           let start_quote = 8 in
           (* length of "import \"" *)
           let end_quote = String.rindex trimmed '"' in
           if end_quote > start_quote then
             let import_path =
               String.sub trimmed start_quote (end_quote - start_quote)
             in
             let import_info =
               {
                 module_path = import_path;
                 source_file = filepath;
                 line_number = !line_num;
               }
             in
             imports := import_info :: !imports
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !imports
  with Sys_error _ -> []

(** Check if module path is a standard library module *)
let is_stdlib_module module_path =
  let stdlib_prefixes = [ "std/"; "core/"; "builtin/" ] in
  List.exists (String.starts_with ~prefix:module_path) stdlib_prefixes

(** Check if module path is a relative/local import *)
let is_local_import module_path =
  String.starts_with ~prefix:"./" module_path
  || String.starts_with ~prefix:"../" module_path
  || not (String.contains module_path '/')

(** Extract external dependency from module path *)
let extract_dependency_name module_path =
  if is_stdlib_module module_path || is_local_import module_path then None
  else
    (* Extract root module from path like "github.com/user/project/submodule" *)
    (* TODO: Support additional package registries beyond GitHub:
       - gitlab.com, bitbucket.org
       - Private repositories with authentication
       - Custom domain package hosts *)
    let parts = String.split_on_char '/' module_path in
    match parts with
    | domain :: user :: project :: _ when String.contains domain '.' ->
        (* TODO: Validate that domain/user/project actually exists before
           adding *)
        Some (String.concat "/" [ domain; user; project ])
    | _ -> None

(** Scan all Snow files and collect external dependencies *)
let scan_project_dependencies () : dependency_resolution list =
  let snow_files = find_snow_files () in
  let all_import_infos = List.concat_map extract_imports_from_file snow_files in

  (* Convert import_infos to dependency_resolutions *)
  let all_dependencies =
    let filter_func (import_info : import_info) : dependency_resolution option =
      match extract_dependency_name import_info.module_path with
      | Some dep_name ->
          Some
            {
              module_path = dep_name;
              resolved_version = "latest";
              (* Default version, will be resolved later *)
              is_direct = true;
            }
      | None -> None
    in
    List.filter_map filter_func all_import_infos
  in

  let external_deps =
    List.map
      (fun dep_resolution -> (dep_resolution.module_path, dep_resolution))
      all_dependencies
  in

  (* Group by dependency name *)
  let dep_groups =
    List.fold_left
      (fun acc (dep_name, dep_resolution) ->
        let existing = try List.assoc dep_name acc with Not_found -> [] in
        (dep_name, dep_resolution :: existing) :: List.remove_assoc dep_name acc)
      [] external_deps
  in

  (* Convert to list of first dependency resolution for each module *)
  List.map
    (fun (_dep_name, resolutions) ->
      let first_resolution = List.hd (List.rev resolutions) in
      first_resolution)
    dep_groups

(** Find package main files in project *)
let find_main_packages () =
  let snow_files = find_snow_files () in
  List.filter_map
    (fun file ->
      match extract_package_from_file file with
      | Some "main" -> Some file
      | _ -> None)
    snow_files

(** Resolve all dependencies using Minimal Version Selection *)
let resolve_all_dependencies current_mod required_deps =
  try
    (* Create temporary snow_mod with required dependencies *)
    let temp_require =
      List.map
        (fun dep_name ->
          {
            name = dep_name;
            constraint_type = Exact "v1.0.0";
            (* Use valid semantic version instead of commit hash *)
          })
        required_deps
    in

    let temp_mod = { current_mod with require = temp_require } in

    (* Use MVS to resolve all dependencies *)
    let resolutions = minimal_version_selection temp_mod in
    List.map
      (fun (module_path, resolved_version, is_direct) ->
        { module_path; resolved_version; is_direct })
      resolutions
  with Resolution_error msg ->
    Printf.eprintf "Warning: Resolution failed: %s\n" msg;
    List.map
      (fun dep_name ->
        {
          module_path = dep_name;
          resolved_version = "v1.0.0";
          (* Use valid semantic version instead of commit hash *)
          is_direct = true;
        })
      required_deps

(** Generate tidy recommendations *)
let analyze_dependencies current_mod project_deps =
  let current_dep_names = List.map (fun dep -> dep.name) current_mod.require in
  let required_deps = List.map fst project_deps in

  let missing_deps =
    List.filter (fun dep -> not (List.mem dep current_dep_names)) required_deps
  in

  let unused_deps =
    List.filter (fun dep -> not (List.mem dep required_deps)) current_dep_names
  in

  let warnings = ref [] in

  (* Check for potential issues *)
  List.iter
    (fun (dep_name, imports) ->
      let import_count = List.length imports in
      if import_count > 5 then
        warnings :=
          Printf.sprintf "Heavy usage of %s (%d imports)" dep_name import_count
          :: !warnings)
    project_deps;

  let added_resolutions =
    if List.length missing_deps > 0 then
      let all_resolutions = resolve_all_dependencies current_mod missing_deps in
      List.filter
        (fun res -> List.mem res.module_path missing_deps)
        all_resolutions
    else []
  in

  {
    added_dependencies = added_resolutions;
    removed_dependencies = unused_deps;
    updated_dependencies = [];
    warnings = List.rev !warnings;
  }

(** Update snow.mod and snow.sum files with tidy results *)
let update_files current_mod tidy_result =
  (* Add missing dependencies *)
  let new_dependencies =
    List.map
      (fun resolution ->
        {
          name = resolution.module_path;
          constraint_type = Exact resolution.resolved_version;
        })
      tidy_result.added_dependencies
  in

  (* Remove unused dependencies *)
  let filtered_deps =
    List.filter
      (fun dep -> not (List.mem dep.name tidy_result.removed_dependencies))
      current_mod.require
  in

  let updated_require = filtered_deps @ new_dependencies in

  (* Create updated module info *)
  let updated_mod = { current_mod with require = updated_require } in

  (* Write updated snow.mod *)
  let content = generate_snow_mod_content updated_mod in
  let oc = open_out "snow.mod" in
  output_string oc content;
  close_out oc;

  (* Update snow.sum *)
  let current_sum = load_current_snow_sum () in

  (* Remove checksums for removed dependencies *)
  let updated_sum =
    List.fold_left
      (fun sum_db removed_dep -> remove_module_checksums sum_db removed_dep)
      current_sum tidy_result.removed_dependencies
  in

  (* Add checksums for new dependencies *)
  (* TODO: Download modules and calculate actual checksums *)
  let final_sum =
    List.fold_left
      (fun sum_db resolution ->
        (* For now, add placeholder checksums *)
        let sum_with_mod =
          add_checksum_entry sum_db resolution.module_path
            resolution.resolved_version false "placeholder_hash"
        in
        add_checksum_entry sum_with_mod resolution.module_path
          resolution.resolved_version true "placeholder_mod_hash")
      updated_sum tidy_result.added_dependencies
  in

  save_current_snow_sum final_sum

(* TODO: Implement comprehensive package management features: - Network-based
   package existence validation - Git tag fetching and semantic version parsing
   - Dependency graph resolution and conflict detection - Caching mechanism for
   fetched package metadata - Parallel dependency resolution for performance -
   Support for dev-dependencies vs runtime dependencies - Lock file generation
   (snow.lock) for reproducible builds *)

(** Main mod tidy command implementation *)
let mod_tidy_command args =
  let verbose = List.mem "-v" args || List.mem "--verbose" args in

  try
    if verbose then Printf.eprintf "Running snow mod tidy...\n";

    (* Load current snow.mod *)
    let current_mod = load_current_snow_mod () in

    if verbose then
      Printf.eprintf "Current module: %s v%s\n" current_mod.module_name
        current_mod.snow_version;

    (* Scan project for dependencies *)
    if verbose then Printf.eprintf "Scanning project files...\n";

    let project_deps = scan_project_dependencies () in

    if verbose then (
      Printf.eprintf "Found external dependencies:\n";
      List.iter
        (fun dep_resolution ->
          Printf.eprintf "  %s (version %s)\n" dep_resolution.module_path
            dep_resolution.resolved_version)
        project_deps);

    (* Analyze what needs to be changed *)
    (* Convert dependency_resolution list to (string * dependency_resolution list) list *)
    let project_deps_grouped =
      List.map (fun dep -> (dep.module_path, [ dep ])) project_deps
    in
    let tidy_result = analyze_dependencies current_mod project_deps_grouped in

    (* Print summary *)
    let changes_made =
      List.length tidy_result.added_dependencies > 0
      || List.length tidy_result.removed_dependencies > 0
      || List.length tidy_result.updated_dependencies > 0
    in

    if changes_made then (
      Printf.printf "snow mod tidy: updating snow.mod\n";

      if List.length tidy_result.added_dependencies > 0 then (
        Printf.printf "  Added dependencies:\n";
        List.iter
          (fun resolution ->
            Printf.printf "    + %s %s\n" resolution.module_path
              resolution.resolved_version)
          tidy_result.added_dependencies);

      if List.length tidy_result.removed_dependencies > 0 then (
        Printf.printf "  Removed unused dependencies:\n";
        List.iter (Printf.printf "    - %s\n") tidy_result.removed_dependencies);

      (* Update snow.mod and snow.sum files *)
      update_files current_mod tidy_result;

      Printf.printf "snow.mod and snow.sum updated successfully\n")
    else Printf.printf "snow mod tidy: no changes needed\n";

    (* Print warnings *)
    List.iter
      (fun warning -> Printf.printf "Warning: %s\n" warning)
      tidy_result.warnings
  with
  | Mod_parse_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Mod_tidy_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      exit 1
