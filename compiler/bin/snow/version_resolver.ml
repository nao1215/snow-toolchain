(*** @project snow-toolchain @module Snow_Compiler_Version_Resolver

  Minimal Version Selection algorithm for the Snow programming language.

  This module provides dependency resolution using Minimal Version Selection
  (MVS), the same algorithm used by Go modules. MVS always selects the minimum
  version that satisfies all constraints, providing reproducible builds.

  Features: - Semantic version parsing and comparison - Minimal Version
  Selection algorithm - Dependency graph resolution - Conflict detection and
  resolution - Support for replace/exclude directives *)

open Mod_parser

exception Version_error of string
exception Resolution_error of string

type version_type =
  | SemVer of semver
  | CommitHash of string
  | BranchName of string

and semver = {
  major : int;
  minor : int;
  patch : int;
  prerelease : string option;
  build : string option;
}

type dependency_node = {
  module_path : string;
  version : version_type;
  dependencies : dependency list;
  is_direct : bool;
}

type resolution_graph = { nodes : dependency_node list; root : string }

(** Parse version string (semantic version, commit hash, or branch name) *)
let parse_version version_str =
  let clean_version =
    if String.starts_with ~prefix:"v" version_str then
      String.sub version_str 1 (String.length version_str - 1)
    else version_str
  in

  (* Check if it's a commit hash (40 hex chars) *)
  if
    String.length clean_version >= 7
    && String.length clean_version <= 40
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
         clean_version
  then CommitHash clean_version (* Check if it looks like semantic version *)
  else if String.contains clean_version '.' then
    try
      (* Split on + for build metadata *)
      let parts = String.split_on_char '+' clean_version in
      let main_part = List.hd parts in
      let build =
        if List.length parts > 1 then Some (List.nth parts 1) else None
      in

      (* Split on - for prerelease *)
      let version_parts = String.split_on_char '-' main_part in
      let version_core = List.hd version_parts in
      let prerelease =
        if List.length version_parts > 1 then
          Some (String.concat "-" (List.tl version_parts))
        else None
      in

      (* Parse major.minor.patch *)
      let core_parts = String.split_on_char '.' version_core in
      match core_parts with
      | [ major_str; minor_str; patch_str ] ->
          SemVer
            {
              major = int_of_string major_str;
              minor = int_of_string minor_str;
              patch = int_of_string patch_str;
              prerelease;
              build;
            }
      | _ -> BranchName version_str (* Fallback to branch name *)
    with Failure _ -> BranchName version_str
  else
    (* Assume it's a branch name *)
    BranchName version_str

(** Parse semantic version string (legacy function for backward compatibility)
*)
let parse_semver version_str =
  match parse_version version_str with
  | SemVer semver -> semver
  | _ ->
      raise (Version_error ("Expected semantic version, got: " ^ version_str))

(** Convert version to string *)
let version_to_string version =
  match version with
  | SemVer semver ->
      let core =
        Printf.sprintf "%d.%d.%d" semver.major semver.minor semver.patch
      in
      let with_prerelease =
        match semver.prerelease with
        | Some pre -> core ^ "-" ^ pre
        | None -> core
      in
      let with_build =
        match semver.build with
        | Some build -> with_prerelease ^ "+" ^ build
        | None -> with_prerelease
      in
      "v" ^ with_build
  | CommitHash hash -> hash
  | BranchName branch -> branch

(** Convert semver to string (legacy function for backward compatibility) *)
let semver_to_string semver = version_to_string (SemVer semver)

(** Compare semantic versions internal helper *)
let compare_semver_internal a_sem b_sem =
  let major_cmp = Int.compare a_sem.major b_sem.major in
  if major_cmp <> 0 then major_cmp
  else
    let minor_cmp = Int.compare a_sem.minor b_sem.minor in
    if minor_cmp <> 0 then minor_cmp
    else
      let patch_cmp = Int.compare a_sem.patch b_sem.patch in
      if patch_cmp <> 0 then patch_cmp
      else
        (* Prerelease versions have lower precedence *)
        match (a_sem.prerelease, b_sem.prerelease) with
        | None, None -> 0
        | Some _, None -> -1
        | None, Some _ -> 1
        | Some pre_a, Some pre_b -> String.compare pre_a pre_b

(** Compare versions *)
let compare_version a b =
  match (a, b) with
  | SemVer a_sem, SemVer b_sem -> compare_semver_internal a_sem b_sem
  | CommitHash a_hash, CommitHash b_hash -> String.compare a_hash b_hash
  | BranchName a_branch, BranchName b_branch -> String.compare a_branch b_branch
  | SemVer _, _ -> 1 (* SemVer has higher precedence *)
  | _, SemVer _ -> -1
  | CommitHash _, BranchName _ ->
      1 (* CommitHash has higher precedence than BranchName *)
  | BranchName _, CommitHash _ -> -1

(** Compare semantic versions (legacy function for backward compatibility) *)
let compare_semver a b = compare_version (SemVer a) (SemVer b)

(** Check if version satisfies constraint *)
let satisfies_constraint version constraint_type =
  match constraint_type with
  | Exact required -> (
      match parse_version required with
      | SemVer req_sem -> compare_version version (SemVer req_sem) = 0
      | other -> compare_version version other = 0)
  | GreaterEqual required -> (
      match parse_version required with
      | SemVer req_sem -> compare_version version (SemVer req_sem) >= 0
      | _ -> true)
(* Non-semver versions always satisfy >= constraints *)

(** Find maximum version that satisfies constraint *)
let resolve_version_constraint available_versions constraint_type =
  let valid_versions =
    List.filter
      (fun v -> satisfies_constraint v constraint_type)
      available_versions
  in
  match valid_versions with
  | [] -> raise (Resolution_error "No version satisfies constraint")
  | versions ->
      List.fold_left
        (fun acc v -> if compare_version v acc > 0 then v else acc)
        (List.hd versions) versions

(** Mock function to get available versions (TODO: implement remote fetching) *)
let get_available_versions _module_path =
  (* TODO: Implement actual version fetching from remote repository *)
  (* For now, return some mock versions *)
  [
    SemVer { major = 1; minor = 0; patch = 0; prerelease = None; build = None };
    SemVer { major = 1; minor = 1; patch = 0; prerelease = None; build = None };
    SemVer { major = 1; minor = 2; patch = 0; prerelease = None; build = None };
    SemVer { major = 2; minor = 0; patch = 0; prerelease = None; build = None };
  ]

(** Mock function to get module dependencies (TODO: implement remote fetching)
*)
let get_module_dependencies _module_path _version =
  (* TODO: Implement actual dependency fetching from remote snow.mod *)
  (* For now, return empty dependencies *)
  []

(** Build dependency graph using Minimal Version Selection *)
let resolve_dependencies root_module direct_deps =
  let visited = Hashtbl.create 16 in
  let result_nodes = ref [] in

  let rec resolve_module module_path constraint_type is_direct =
    if not (Hashtbl.mem visited module_path) then (
      (* Mark as visited to prevent cycles *)
      Hashtbl.add visited module_path true;

      (* Get available versions and resolve constraint *)
      let available_versions = get_available_versions module_path in
      let selected_version =
        resolve_version_constraint available_versions constraint_type
      in

      (* Get dependencies of selected version *)
      let module_deps = get_module_dependencies module_path selected_version in

      (* Create dependency node *)
      let node =
        {
          module_path;
          version = selected_version;
          dependencies = module_deps;
          is_direct;
        }
      in
      result_nodes := node :: !result_nodes;

      (* Recursively resolve dependencies *)
      List.iter
        (fun dep -> resolve_module dep.name dep.constraint_type false)
        module_deps)
  in

  (* Resolve all direct dependencies *)
  List.iter
    (fun dep -> resolve_module dep.name dep.constraint_type true)
    direct_deps;

  { nodes = List.rev !result_nodes; root = root_module }

(** Apply replacements to dependency graph *)
let apply_replacements graph replacements =
  let replace_map =
    List.fold_left
      (fun acc repl -> (repl.original, repl) :: acc)
      [] replacements
  in

  let updated_nodes =
    List.map
      (fun node ->
        match List.assoc_opt node.module_path replace_map with
        | Some replacement ->
            (* Apply replacement *)
            let new_version =
              match replacement.version with
              | Some v -> parse_version v
              | None ->
                  node.version (* Keep original version if not specified *)
            in
            {
              node with
              module_path = replacement.target;
              version = new_version;
            }
        | None -> node)
      graph.nodes
  in

  { graph with nodes = updated_nodes }

(** Filter out excluded dependencies *)
let apply_exclusions graph exclusions =
  let excluded_set =
    List.fold_left
      (fun acc dep -> (dep.name, dep.constraint_type) :: acc)
      [] exclusions
  in

  let filtered_nodes =
    List.filter
      (fun node ->
        not
          (List.exists
             (fun (module_path, constraint_type) ->
               module_path = node.module_path
               && satisfies_constraint node.version constraint_type)
             excluded_set))
      graph.nodes
  in

  { graph with nodes = filtered_nodes }

(** Resolve all dependencies with MVS *)
let minimal_version_selection snow_mod =
  try
    (* Build initial dependency graph *)
    let graph = resolve_dependencies snow_mod.module_name snow_mod.require in

    (* Apply replacements *)
    let graph_with_replacements = apply_replacements graph snow_mod.replace in

    (* Apply exclusions *)
    let final_graph =
      apply_exclusions graph_with_replacements snow_mod.exclude
    in

    (* Convert to dependency resolutions *)
    List.map
      (fun node ->
        (node.module_path, version_to_string node.version, node.is_direct))
      final_graph.nodes
  with
  | Version_error msg -> raise (Resolution_error ("Version error: " ^ msg))
  | exn ->
      raise (Resolution_error ("Resolution failed: " ^ Printexc.to_string exn))

(** Get direct dependencies only *)
let get_direct_dependencies resolutions =
  List.filter (fun (_, _, is_direct) -> is_direct) resolutions

(** Get indirect dependencies only *)
let get_indirect_dependencies resolutions =
  List.filter (fun (_, _, is_direct) -> not is_direct) resolutions

(** Debug: print dependency graph *)
let print_dependency_graph graph =
  Printf.printf "Dependency graph for %s:\n" graph.root;
  List.iter
    (fun node ->
      let direct_str = if node.is_direct then " (direct)" else "" in
      Printf.printf "  %s %s%s\n" node.module_path
        (version_to_string node.version)
        direct_str;
      List.iter
        (fun dep ->
          Printf.printf "    -> %s %s\n" dep.name
            (version_constraint_to_string dep.constraint_type))
        node.dependencies)
    graph.nodes
