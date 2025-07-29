open Alcotest

(** Test module for snow install command functionality using mocks *)

(** Mock module for testing install functionality without network access *)
module Mock_install = struct
  type version_spec =
    | Latest
    | Tag of string
    | Branch of string
    | Commit of string

  (** Parse module path with optional version - pure function, no side effects
  *)
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
      | _ -> failwith ("Invalid module specification: " ^ module_spec)
    else (module_spec, Latest)

  (** Convert module path to git URL - pure function *)
  let module_path_to_git_url module_path =
    if String.starts_with ~prefix:"github.com/" module_path then
      "https://" ^ module_path ^ ".git"
    else if String.starts_with ~prefix:"gitlab.com/" module_path then
      "https://" ^ module_path ^ ".git"
    else failwith ("Unsupported package registry: " ^ module_path)

  (** Mock environment variable handling *)
  let mock_get_snow_path custom_home =
    match custom_home with Some home -> home ^ "/snow" | None -> "./snow"

  (** Mock binary name extraction *)
  let mock_get_binary_name module_path has_snow_mod custom_bin_name =
    if has_snow_mod then
      match custom_bin_name with
      | Some name -> name
      | None -> Filename.basename module_path
    else Filename.basename module_path

  (** Mock version string generation *)
  let get_version_string _module_path version =
    match version with
    | Latest -> "latest"
    | Tag tag -> tag
    | Branch branch -> branch
    | Commit commit -> commit

  (** Mock cache directory path generation *)
  let mock_get_module_version_dir snow_path module_path version_str =
    let safe_module_path =
      String.map (function '/' -> '/' | c -> c) module_path
    in
    Printf.sprintf "%s/pkg/mod/%s@%s" snow_path safe_module_path version_str
end

(** Test module path parsing *)
let test_parse_module_spec () =
  let open Mock_install in
  (* Test parsing module path without version *)
  let module_path, version = parse_module_spec "github.com/user/tool" in
  check string "module_path" "github.com/user/tool" module_path;
  (match version with Latest -> () | _ -> fail "Expected Latest version");

  (* Test parsing module path with version tag *)
  let module_path, version = parse_module_spec "github.com/user/tool@v1.2.3" in
  check string "module_path" "github.com/user/tool" module_path;
  (match version with Tag "v1.2.3" -> () | _ -> fail "Expected Tag version");

  (* Test parsing module path with branch *)
  let module_path, version = parse_module_spec "github.com/user/tool@main" in
  check string "module_path" "github.com/user/tool" module_path;
  (match version with
  | Branch "main" -> ()
  | _ -> fail "Expected Branch version");

  (* Test parsing module path with commit hash *)
  let module_path, version =
    parse_module_spec "github.com/user/tool@abc1234567890def"
  in
  check string "module_path" "github.com/user/tool" module_path;
  match version with
  | Commit "abc1234567890def" -> ()
  | _ -> fail "Expected Commit version"

(** Test git URL conversion using mock *)
let test_module_path_to_git_url () =
  let open Mock_install in
  (* Test GitHub URL conversion *)
  let git_url = module_path_to_git_url "github.com/user/tool" in
  check string "github_url" "https://github.com/user/tool.git" git_url;

  (* Test GitLab URL conversion *)
  let git_url = module_path_to_git_url "gitlab.com/user/tool" in
  check string "gitlab_url" "https://gitlab.com/user/tool.git" git_url

(** Test snow path handling using mock *)
let test_get_snow_path () =
  let open Mock_install in
  (* Test default snow path with custom home *)
  let snow_path = mock_get_snow_path (Some "/home/testuser") in
  check string "snow_path_with_home" "/home/testuser/snow" snow_path;

  (* Test default snow path without home *)
  let snow_path = mock_get_snow_path None in
  check string "snow_path_default" "./snow" snow_path

(** Test binary name extraction using mock *)
let test_get_binary_name () =
  let open Mock_install in
  (* Test binary name from module path when no snow.mod exists *)
  let binary_name = mock_get_binary_name "github.com/user/mytool" false None in
  check string "binary_name_from_path" "mytool" binary_name;

  (* Test binary name from snow.mod with custom name *)
  let binary_name =
    mock_get_binary_name "github.com/user/mytool" true (Some "custom-tool-name")
  in
  check string "binary_name_from_mod_custom" "custom-tool-name" binary_name;

  (* Test binary name from snow.mod without custom name *)
  let binary_name = mock_get_binary_name "github.com/user/mytool" true None in
  check string "binary_name_from_mod_default" "mytool" binary_name

(** Test error handling for unsupported hosting using mock *)
let test_unsupported_hosting () =
  let open Mock_install in
  (* Test unsupported hosting service *)
  try
    let _ = module_path_to_git_url "example.com/user/tool" in
    fail "Should have thrown exception for unsupported hosting"
  with Failure msg -> (
    try
      ignore (Str.search_forward (Str.regexp "Unsupported") msg 0);
      check bool "error_message_contains_unsupported" true true
    with
    | Not_found -> check bool "error_message_contains_unsupported" false true
    | _ -> fail "Wrong exception type")

(** Test cache directory path generation using mock *)
let test_get_module_version_dir () =
  let open Mock_install in
  (* Test cache directory path generation *)
  let cache_dir =
    mock_get_module_version_dir "/test/snow" "github.com/user/tool" "v1.2.3"
  in
  let expected = "/test/snow/pkg/mod/github.com/user/tool@v1.2.3" in
  check string "cache_dir_path" expected cache_dir

(** Test version string generation using mock *)
let test_get_version_string () =
  let open Mock_install in
  (* Test tag version *)
  let version_str = get_version_string "github.com/user/tool" (Tag "v1.2.3") in
  check string "tag_version" "v1.2.3" version_str;

  (* Test branch version *)
  let version_str = get_version_string "github.com/user/tool" (Branch "main") in
  check string "branch_version" "main" version_str;

  (* Test commit version *)
  let version_str =
    get_version_string "github.com/user/tool" (Commit "abc123def456")
  in
  check string "commit_version" "abc123def456" version_str;

  (* Test latest version *)
  let version_str = get_version_string "github.com/user/tool" Latest in
  check string "latest_version" "latest" version_str

(** Test invalid module specification handling *)
let test_invalid_module_spec () =
  let open Mock_install in
  (* Test invalid module specification with multiple @ symbols *)
  try
    let _ = parse_module_spec "github.com/user/tool@v1.0.0@extra" in
    fail "Should have thrown exception for invalid module spec"
  with Failure msg -> (
    try
      ignore (Str.search_forward (Str.regexp "Invalid") msg 0);
      check bool "error_message_contains_invalid" true true
    with
    | Not_found -> check bool "error_message_contains_invalid" false true
    | _ -> fail "Wrong exception type")

(** Test edge cases for module path parsing *)
let test_edge_cases () =
  let open Mock_install in
  (* Test single character version *)
  let module_path, version = parse_module_spec "github.com/user/tool@v1" in
  check string "single_char_version_path" "github.com/user/tool" module_path;
  (match version with
  | Tag "v1" -> ()
  | _ -> fail "Expected Tag version for single char");

  (* Test branch that looks like commit but is too short *)
  let module_path, version = parse_module_spec "github.com/user/tool@abc123" in
  check string "short_hash_path" "github.com/user/tool" module_path;
  match version with
  | Branch "abc123" -> ()
  | _ -> fail "Expected Branch version for short hash"

(** Main test suite *)
let () =
  let open Alcotest in
  run "Snow Install Tests"
    [
      ( "module_spec_parsing",
        [
          test_case "parse module spec" `Quick test_parse_module_spec;
          test_case "invalid module spec" `Quick test_invalid_module_spec;
          test_case "edge cases" `Quick test_edge_cases;
        ] );
      ( "git_url_conversion",
        [
          test_case "module path to git url" `Quick test_module_path_to_git_url;
          test_case "unsupported hosting error" `Quick test_unsupported_hosting;
        ] );
      ( "environment_handling",
        [ test_case "snow path detection" `Quick test_get_snow_path ] );
      ( "binary_naming",
        [ test_case "binary name extraction" `Quick test_get_binary_name ] );
      ( "cache_management",
        [ test_case "cache directory path" `Quick test_get_module_version_dir ]
      );
      ( "version_handling",
        [ test_case "version string generation" `Quick test_get_version_string ]
      );
    ]
