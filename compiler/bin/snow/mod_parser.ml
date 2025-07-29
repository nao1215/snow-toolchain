(*** @project snow-toolchain @module Snow_Compiler_Mod_Parser

  Module configuration file parser for the Snow programming language.

  This module provides functionality to parse and manipulate snow.mod files,
  which are YAML-based configuration files that describe Snow modules and their
  dependencies, similar to go.mod files in Go.

  Features: - Parse snow.mod YAML files into structured data - Extract module
  information and dependencies - Support for development dependencies and
  metadata - Version constraint parsing and validation - Error handling with
  detailed error messages *)

exception Mod_parse_error of string

type version_constraint = Exact of string | GreaterEqual of string
type dependency = { name : string; constraint_type : version_constraint }

type replacement = {
  original : string;
  target : string;
  version : string option;
}

type retraction = { version : string; rationale : string }

type snow_mod = {
  module_name : string;
  snow_version : string; (* minimum required Snow version *)
  require : dependency list; (* direct dependencies only *)
  replace : replacement list;
  exclude : dependency list;
  retract : retraction list;
}

(** Parse version constraint from string *)
let parse_version_constraint version_str =
  let trimmed = String.trim version_str in
  if String.starts_with ~prefix:">=" trimmed then
    GreaterEqual
      (String.sub trimmed 2 (String.length trimmed - 2) |> String.trim)
  else Exact trimmed

(** Parse dependencies from YAML node *)
let parse_dependencies deps_node =
  match deps_node with
  | `O assocs ->
      List.map
        (fun (name, version_node) ->
          let version_str =
            match version_node with
            | `String s -> s
            | _ ->
                raise
                  (Mod_parse_error
                     ("Invalid version format for dependency: " ^ name))
          in
          { name; constraint_type = parse_version_constraint version_str })
        assocs
  | `Null -> []
  | _ -> raise (Mod_parse_error "Dependencies must be an object or null")

(** Find string value in YAML associations *)
let find_string_exn assocs key =
  match List.assoc_opt key assocs with
  | Some (`String s) -> s
  | Some _ -> raise (Mod_parse_error (key ^ " must be a string"))
  | None -> raise (Mod_parse_error (key ^ " is required"))

(** Find string or numeric value in YAML associations, convert to string *)
let find_string_or_number_exn assocs key =
  match List.assoc_opt key assocs with
  | Some (`String s) -> s
  | Some (`Float f) ->
      (* Remove .0 if it's a whole number *)
      let fs = string_of_float f in
      if String.ends_with ~suffix:".0" fs then
        String.sub fs 0 (String.length fs - 2)
      else fs
  | Some _ -> raise (Mod_parse_error (key ^ " must be a string or number"))
  | None -> raise (Mod_parse_error (key ^ " is required"))

let find_string_opt assocs key =
  match List.assoc_opt key assocs with
  | Some (`String s) -> Some s
  | Some _ -> raise (Mod_parse_error (key ^ " must be a string"))
  | None -> None

(** Parse replacements from YAML node *)
let parse_replacements replace_node =
  match replace_node with
  | `O assocs ->
      List.map
        (fun (original, replacement_node) ->
          match replacement_node with
          | `String replacement -> (
              (* Format: "github.com/new/lib v1.0.0" or "../local/path" *)
              let parts = String.split_on_char ' ' replacement in
              match parts with
              | [ path ] -> { original; target = path; version = None }
              | path :: version :: _ ->
                  { original; target = path; version = Some version }
              | [] ->
                  raise
                    (Mod_parse_error
                       ("Invalid replacement format for: " ^ original)))
          | _ ->
              raise
                (Mod_parse_error ("Invalid replacement format for: " ^ original)))
        assocs
  | `Null -> []
  | _ -> raise (Mod_parse_error "Replace must be an object or null")

(** Parse retractions from YAML node *)
let parse_retractions retract_node =
  match retract_node with
  | `A items ->
      List.map
        (fun item ->
          match item with
          | `O assocs ->
              let version = find_string_exn assocs "version" in
              let rationale = find_string_exn assocs "rationale" in
              { version; rationale }
          | _ -> raise (Mod_parse_error "Each retraction must be an object"))
        items
  | `Null -> []
  | _ -> raise (Mod_parse_error "Retract must be an array or null")

(** Parse snow.mod file content *)
let parse_snow_mod content =
  try
    match Yaml.of_string content with
    | Ok yaml_doc -> (
        match yaml_doc with
        | `O assocs ->
            let module_name = find_string_exn assocs "module" in
            let snow_version =
              match List.assoc_opt "snow" assocs with
              | Some version_node -> (
                  match version_node with
                  | `String s -> s
                  | `Float f ->
                      let fs = string_of_float f in
                      if String.ends_with ~suffix:".0" fs then
                        String.sub fs 0 (String.length fs - 2)
                      else fs
                  | _ ->
                      raise (Mod_parse_error "snow must be a string or number"))
              | None ->
                  (* Fallback to "version" field for compatibility *)
                  find_string_or_number_exn assocs "version"
            in

            let require =
              match List.assoc_opt "require" assocs with
              | Some deps_node -> parse_dependencies deps_node
              | None -> []
            in

            let replace =
              match List.assoc_opt "replace" assocs with
              | Some replace_node -> parse_replacements replace_node
              | None -> []
            in

            let exclude =
              match List.assoc_opt "exclude" assocs with
              | Some exclude_node -> parse_dependencies exclude_node
              | None -> []
            in

            let retract =
              match List.assoc_opt "retract" assocs with
              | Some retract_node -> parse_retractions retract_node
              | None -> []
            in

            { module_name; snow_version; require; replace; exclude; retract }
        | _ -> raise (Mod_parse_error "snow.mod must contain a YAML object"))
    | Error (`Msg msg) -> raise (Mod_parse_error ("YAML parse error: " ^ msg))
  with
  | Mod_parse_error _ as e -> raise e
  | exn ->
      raise (Mod_parse_error ("Unexpected error: " ^ Printexc.to_string exn))

(** Load snow.mod file from filesystem *)
let load_snow_mod_file path =
  try
    let content =
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    parse_snow_mod content
  with Sys_error msg -> raise (Mod_parse_error ("File error: " ^ msg))

(** Check if snow.mod exists in current or parent directories *)
let rec find_snow_mod_path dir =
  let snow_mod_path = Filename.concat dir "snow.mod" in
  if Sys.file_exists snow_mod_path then Some snow_mod_path
  else
    let parent = Filename.dirname dir in
    if parent = dir then None (* reached filesystem root *)
    else find_snow_mod_path parent

let snow_mod_exists () =
  match find_snow_mod_path (Sys.getcwd ()) with Some _ -> true | None -> false

(** Load snow.mod from current or parent directories *)
let load_current_snow_mod () =
  match find_snow_mod_path (Sys.getcwd ()) with
  | Some path -> load_snow_mod_file path
  | None ->
      raise
        (Mod_parse_error "snow.mod not found in current or parent directories")

(** Convert version constraint to string *)
let version_constraint_to_string = function
  | Exact v -> v
  | GreaterEqual v -> ">= " ^ v

(** Convert dependency to string *)
let dependency_to_string dep =
  Printf.sprintf "%s: %s" dep.name
    (version_constraint_to_string dep.constraint_type)

(** Generate snow.mod YAML content *)
let generate_snow_mod_content mod_info =
  let require_content =
    if List.length mod_info.require > 0 then
      let dep_lines =
        List.map
          (fun dep ->
            Printf.sprintf "  %s: %s" dep.name
              (version_constraint_to_string dep.constraint_type))
          mod_info.require
      in
      "require:\n" ^ String.concat "\n" dep_lines ^ "\n\n"
    else "require: {}\n\n"
  in

  let replace_content =
    if List.length mod_info.replace > 0 then
      let replace_lines =
        List.map
          (fun (repl : replacement) ->
            let replacement_str =
              match repl.version with
              | Some v -> repl.target ^ " " ^ v
              | None -> repl.target
            in
            Printf.sprintf "  %s: %s" repl.original replacement_str)
          mod_info.replace
      in
      "replace:\n" ^ String.concat "\n" replace_lines ^ "\n\n"
    else ""
  in

  let exclude_content =
    if List.length mod_info.exclude > 0 then
      let exclude_lines =
        List.map
          (fun dep ->
            Printf.sprintf "  %s: %s" dep.name
              (version_constraint_to_string dep.constraint_type))
          mod_info.exclude
      in
      "exclude:\n" ^ String.concat "\n" exclude_lines ^ "\n\n"
    else ""
  in

  let retract_content =
    if List.length mod_info.retract > 0 then
      let retract_lines =
        List.map
          (fun ret ->
            Printf.sprintf "  - version: %s\n    rationale: \"%s\"" ret.version
              ret.rationale)
          mod_info.retract
      in
      "retract:\n" ^ String.concat "\n" retract_lines ^ "\n\n"
    else ""
  in

  Printf.sprintf {|# Snow module file
module: %s
snow: "%s"

%s%s%s%s|}
    mod_info.module_name mod_info.snow_version require_content replace_content
    exclude_content retract_content

(** Convert snow_mod to string for debugging *)
let snow_mod_to_string mod_info =
  let deps_str =
    String.concat ", " (List.map dependency_to_string mod_info.require)
  in
  Printf.sprintf
    "Module: %s (snow %s)\nRequire: [%s]\nReplace: %d\nExclude: %d\nRetract: %d"
    mod_info.module_name mod_info.snow_version deps_str
    (List.length mod_info.replace)
    (List.length mod_info.exclude)
    (List.length mod_info.retract)
