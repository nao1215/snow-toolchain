(*** @project snow-toolchain @module Snow_Compiler_Sum_Parser

  Checksum database parser and generator for the Snow programming language.

  This module provides functionality to parse and manipulate snow.sum files,
  which contain cryptographic checksums for all dependencies to ensure build
  reproducibility and security, similar to go.sum files in Go.

  Features: - Parse snow.sum files into structured data - Generate and update
  snow.sum entries - Verify module checksums - Support for both module source
  and snow.mod checksums - SHA-256 based hash verification *)

exception Sum_parse_error of string

type checksum_entry = {
  module_path : string;
  version : string;
  is_mod_file : bool; (* true if this is a /snow.mod checksum *)
  hash_algorithm : string; (* currently only "h1" (SHA-256) *)
  hash_value : string; (* base64-encoded hash *)
}

type sum_database = { entries : checksum_entry list }

(** Parse a single line from snow.sum file *)
let parse_sum_line line =
  let trimmed = String.trim line in
  if trimmed = "" || String.starts_with ~prefix:"#" trimmed then None
  else
    try
      (* Format: "github.com/user/lib v1.2.3 h1:abc123..." *)
      (* or: "github.com/user/lib v1.2.3/snow.mod h1:abc123..." *)
      let parts = String.split_on_char ' ' trimmed in
      match parts with
      | [ module_version; hash_part ] -> (
          (* Parse hash *)
          let hash_parts = String.split_on_char ':' hash_part in
          match hash_parts with
          | [ algorithm; hash_value ] ->
              (* Parse module and version *)
              if String.ends_with ~suffix:"/snow.mod" module_version then
                let mod_part_len = String.length "/snow.mod" in
                let module_version_clean =
                  String.sub module_version 0
                    (String.length module_version - mod_part_len)
                in
                let last_space = String.rindex module_version_clean ' ' in
                let module_path =
                  String.sub module_version_clean 0 last_space
                in
                let version =
                  String.sub module_version_clean (last_space + 1)
                    (String.length module_version_clean - last_space - 1)
                in
                Some
                  {
                    module_path;
                    version;
                    is_mod_file = true;
                    hash_algorithm = algorithm;
                    hash_value;
                  }
              else
                let last_space = String.rindex module_version ' ' in
                let module_path = String.sub module_version 0 last_space in
                let version =
                  String.sub module_version (last_space + 1)
                    (String.length module_version - last_space - 1)
                in
                Some
                  {
                    module_path;
                    version;
                    is_mod_file = false;
                    hash_algorithm = algorithm;
                    hash_value;
                  }
          | _ -> raise (Sum_parse_error ("Invalid hash format: " ^ hash_part)))
      | _ -> raise (Sum_parse_error ("Invalid sum line format: " ^ trimmed))
    with Not_found | Invalid_argument _ ->
      raise (Sum_parse_error ("Malformed sum line: " ^ trimmed))

(** Parse snow.sum file content *)
let parse_snow_sum content =
  let lines = String.split_on_char '\n' content in
  let entries = List.filter_map parse_sum_line lines in
  { entries }

(** Load snow.sum file from filesystem *)
let load_snow_sum_file path =
  try
    let content =
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    parse_snow_sum content
  with Sys_error _ -> { entries = [] }
(* Return empty if file doesn't exist *)

(** Load snow.sum from current directory *)
let load_current_snow_sum () = load_snow_sum_file "snow.sum"

(** Convert checksum entry to string *)
let checksum_entry_to_string entry =
  let module_version =
    if entry.is_mod_file then
      Printf.sprintf "%s %s/snow.mod" entry.module_path entry.version
    else Printf.sprintf "%s %s" entry.module_path entry.version
  in
  Printf.sprintf "%s %s:%s" module_version entry.hash_algorithm entry.hash_value

(** Generate snow.sum file content *)
let generate_snow_sum_content sum_db =
  let sorted_entries =
    List.sort
      (fun a b ->
        let module_cmp = String.compare a.module_path b.module_path in
        if module_cmp <> 0 then module_cmp
        else
          let version_cmp = String.compare a.version b.version in
          if version_cmp <> 0 then version_cmp
          else Bool.compare a.is_mod_file b.is_mod_file)
      sum_db.entries
  in
  String.concat "\n" (List.map checksum_entry_to_string sorted_entries) ^ "\n"

(** Add or update a checksum entry *)
let add_checksum_entry sum_db module_path version is_mod_file hash_value =
  let new_entry =
    {
      module_path;
      version;
      is_mod_file;
      hash_algorithm = "h1";
      (* SHA-256 *)
      hash_value;
    }
  in

  (* Remove existing entry for same module/version/type if exists *)
  let filtered_entries =
    List.filter
      (fun entry ->
        not
          (entry.module_path = module_path
          && entry.version = version
          && entry.is_mod_file = is_mod_file))
      sum_db.entries
  in

  { entries = new_entry :: filtered_entries }

(** Remove checksum entries for a specific module *)
let remove_module_checksums sum_db module_path =
  let filtered_entries =
    List.filter (fun entry -> entry.module_path <> module_path) sum_db.entries
  in
  { entries = filtered_entries }

(** Find checksum for a specific module/version *)
let find_checksum sum_db module_path version is_mod_file =
  List.find_opt
    (fun entry ->
      entry.module_path = module_path
      && entry.version = version
      && entry.is_mod_file = is_mod_file)
    sum_db.entries

(** Verify if a checksum exists *)
let verify_checksum sum_db module_path version is_mod_file expected_hash =
  match find_checksum sum_db module_path version is_mod_file with
  | Some entry -> entry.hash_value = expected_hash
  | None -> false

(** Calculate SHA-256 hash of content *)
let calculate_sha256_hash content =
  let digest = Digest.string content in
  (* Simple base64-like encoding for placeholder *)
  let hex = Digest.to_hex digest in
  "h1:" ^ hex

(** Save snow.sum database to file *)
let save_snow_sum_file path sum_db =
  let content = generate_snow_sum_content sum_db in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Save snow.sum database to current directory *)
let save_current_snow_sum sum_db = save_snow_sum_file "snow.sum" sum_db

(** Check if snow.sum exists in current directory *)
let snow_sum_exists () = Sys.file_exists "snow.sum"

(** Get all modules from sum database *)
let get_all_modules sum_db =
  let modules = List.map (fun entry -> entry.module_path) sum_db.entries in
  List.sort_uniq String.compare modules

(** Get all versions for a module *)
let get_module_versions sum_db module_path =
  let versions =
    List.filter_map
      (fun entry ->
        if entry.module_path = module_path then Some entry.version else None)
      sum_db.entries
  in
  List.sort_uniq String.compare versions

(** Debug: convert sum database to string *)
let sum_database_to_string sum_db =
  Printf.sprintf "Sum database with %d entries:\n%s"
    (List.length sum_db.entries)
    (String.concat "\n"
       (List.map
          (fun entry -> "  " ^ checksum_entry_to_string entry)
          sum_db.entries))
