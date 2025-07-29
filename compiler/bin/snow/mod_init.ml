(*** @project snow-toolchain @module Snow_Compiler_Mod_Init

  Module initialization command for the Snow programming language.

  This module provides the 'snow mod init' subcommand that initializes a new
  Snow module with a snow.mod file, similar to 'go mod init' in Go.

  Features: - Create snow.mod file with module metadata - Auto-detect module
  name from directory or git remote - YAML format for simplicity and readability
  - Compatible with Go-style module naming conventions *)

exception Mod_init_error of string

type module_info = {
  name : string;
  version : string;
  description : string;
  author : string;
  license : string;
}

let default_module_info name =
  { name; version = "0.1.0"; description = ""; author = ""; license = "MIT" }

(** Get current directory name *)
let get_current_dir_name () =
  let cwd = Sys.getcwd () in
  Filename.basename cwd

(** Try to get git remote URL *)
let get_git_remote_url () =
  try
    let cmd = "git remote get-url origin 2>/dev/null" in
    let ic = Unix.open_process_in cmd in
    let url = input_line ic in
    let _ = Unix.close_process_in ic in

    (* Convert git URL to module path *)
    let url = String.trim url in
    if String.length url > 0 then
      (* Handle github.com URLs *)
      if
        Str.string_match
          (Str.regexp "https://github.com/\\([^/]+\\)/\\([^/]+\\)")
          url 0
      then
        let user = Str.matched_group 1 url in
        let repo = Str.matched_group 2 url in
        let repo =
          if String.ends_with ~suffix:".git" repo then
            String.sub repo 0 (String.length repo - 4)
          else repo
        in
        Some ("github.com/" ^ user ^ "/" ^ repo)
      else if
        Str.string_match
          (Str.regexp "git@github.com:\\([^/]+\\)/\\([^/]+\\)")
          url 0
      then
        let user = Str.matched_group 1 url in
        let repo = Str.matched_group 2 url in
        let repo =
          if String.ends_with ~suffix:".git" repo then
            String.sub repo 0 (String.length repo - 4)
          else repo
        in
        Some ("github.com/" ^ user ^ "/" ^ repo)
      else None
    else None
  with _ -> None

(** Auto-detect module name *)
let auto_detect_module_name () =
  match get_git_remote_url () with
  | Some url -> url
  | None -> get_current_dir_name ()

(** Get git user info *)
let get_git_user_info () =
  try
    let get_config key =
      let cmd = Printf.sprintf "git config --get %s 2>/dev/null" key in
      let ic = Unix.open_process_in cmd in
      let value = input_line ic in
      let _ = Unix.close_process_in ic in
      String.trim value
    in
    let name = get_config "user.name" in
    let email = get_config "user.email" in
    if String.length name > 0 && String.length email > 0 then
      Some (Printf.sprintf "%s <%s>" name email)
    else if String.length name > 0 then Some name
    else None
  with _ -> None

(** Generate snow.mod content *)
let generate_snow_mod_content module_info =
  let author_line =
    match module_info.author with
    | "" -> ""
    | author -> Printf.sprintf "author: %s\n" author
  in
  let description_lines =
    match module_info.description with
    | "" -> ""
    | desc -> Printf.sprintf "description: |\n  %s\n" desc
  in

  Printf.sprintf
    {|# Snow module file
# This file describes the module and its dependencies

module: %s
version: %s
%s%slicense: %s

# Dependencies (will be populated by 'snow mod tidy')
dependencies: {}

# Development dependencies (for testing, etc.)
dev-dependencies: {}

# Module metadata
metadata:
  min-snow-version: "0.1.0"
  build-system: "snow"
|}
    module_info.name module_info.version author_line description_lines
    module_info.license

(** Check if snow.mod already exists *)
let check_existing_snow_mod () = Sys.file_exists "snow.mod"

(** Write snow.mod file *)
let write_snow_mod_file content =
  let oc = open_out "snow.mod" in
  output_string oc content;
  close_out oc

(** Interactive prompt for missing information *)
let prompt_for_info prompt default =
  Printf.printf "%s" prompt;
  if default <> "" then Printf.printf " [%s]" default;
  Printf.printf ": ";
  flush stdout;
  let input = read_line () in
  if String.trim input = "" then default else String.trim input

(** Parse mod init arguments *)
let parse_mod_init_args args =
  let rec parse module_name args =
    match args with
    | [] -> (module_name, false, false) (* module_name, interactive, force *)
    | "-i" :: rest | "--interactive" :: rest ->
        let name, _, force = parse module_name rest in
        (name, true, force)
    | "-f" :: rest | "--force" :: rest ->
        let name, interactive, _ = parse module_name rest in
        (name, interactive, true)
    | name :: rest when module_name = "" -> parse name rest
    | unknown :: _ -> raise (Mod_init_error ("Unknown option: " ^ unknown))
  in
  parse "" args

(** Main mod init command implementation *)
let mod_init_command args =
  let explicit_module_name, interactive, force = parse_mod_init_args args in

  (* Check if snow.mod already exists *)
  if check_existing_snow_mod () && not force then
    raise (Mod_init_error "snow.mod already exists (use --force to overwrite)");

  (* Determine module name *)
  let module_name =
    match explicit_module_name with
    | "" -> auto_detect_module_name ()
    | name -> name
  in

  (* Create initial module info *)
  let initial_info = default_module_info module_name in

  (* Get git user info for author *)
  let author =
    match get_git_user_info () with Some author -> author | None -> ""
  in
  let module_info = { initial_info with author } in

  (* Interactive mode: prompt for additional information *)
  let final_info =
    if interactive then
      let name = prompt_for_info "Module name" module_info.name in
      let version = prompt_for_info "Version" module_info.version in
      let description = prompt_for_info "Description" module_info.description in
      let author = prompt_for_info "Author" module_info.author in
      let license = prompt_for_info "License" module_info.license in
      { name; version; description; author; license }
    else module_info
  in

  (* Generate and write snow.mod *)
  let content = generate_snow_mod_content final_info in
  write_snow_mod_file content;

  Printf.printf "Initialized Snow module: %s\n" final_info.name;
  Printf.printf "Created snow.mod file\n";

  (* Determine if this is a library or executable project *)
  let contains_substring s substr =
    let len_s = String.length s in
    let len_substr = String.length substr in
    let rec check i =
      if i > len_s - len_substr then false
      else if String.sub s i len_substr = substr then true
      else check (i + 1)
    in
    len_substr <= len_s && check 0
  in

  let is_library =
    let name_lower =
      String.lowercase_ascii (Filename.basename final_info.name)
    in
    name_lower = "std"
    || contains_substring name_lower "lib"
    || contains_substring name_lower "core"
    || contains_substring name_lower "util"
    ||
    (* GitHub-style library names often don't have main packages *)
    String.contains final_info.name '/'
  in

  (* Show appropriate next steps *)
  Printf.printf "\nNext steps:\n";
  if is_library then (
    Printf.printf "  1. Create library modules (e.g., src/mymodule.sw)\n";
    Printf.printf "  2. Export public functions and types\n";
    Printf.printf "  3. Use 'snow mod tidy' to manage dependencies\n";
    Printf.printf "  4. Other projects can import your library modules\n")
  else (
    Printf.printf "  1. Create your main Snow source file (main.sw)\n";
    Printf.printf "  2. Use 'snow mod tidy' to manage dependencies\n";
    Printf.printf "  3. Use 'snow build' to compile your executable\n";
    Printf.printf "  4. Use 'snow run' to compile and run immediately\n")
