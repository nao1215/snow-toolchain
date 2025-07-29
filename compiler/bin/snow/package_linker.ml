(*** @project snow-toolchain @module Snow_Compiler_Package_Linker

  Package linking system for the Snow programming language.

  This module provides functionality to parse imported packages and integrate
  their definitions into the main program's scope, enabling access to package
  functions and types through qualified names like "fmt.Println".

  Features: - Parse package source files into ASTs - Extract package exports
  (functions, types, constants) - Create qualified namespaces for package
  members - Integrate package definitions into main program scope - Handle
  package-level scoping and visibility *)

open Ast
open Package_resolver

exception Package_link_error of string

type package_member = {
  name : string;
  qualified_name : string; (* e.g., "fmt.Println" *)
  member_type : package_member_type;
}

and package_member_type =
  | Function of func
  | Variable of typ * expr (* type and initialization expression *)
  | Type of typ

type linked_package = {
  package_name : string;
  import_path : string;
  source_file : string;
  members : package_member list;
  ast : program;
}

type package_context = {
  imported_packages : linked_package list;
  qualified_functions : (string * func) list; (* qualified_name -> function *)
  qualified_variables : (string * typ * expr) list;
      (* qualified_name -> type, expr *)
}

(** Parse a package source file *)
let parse_package_file file_path import_path =
  try
    let content =
      let ic = open_in file_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in

    let tokens = Lexer.lex content in
    match Parser.parse_program_with_error_handling tokens with
    | Ok program -> program
    | Error msg ->
        raise
          (Package_link_error
             (Printf.sprintf "Parse error in package %s: %s" import_path msg))
  with
  | Sys_error msg ->
      raise
        (Package_link_error
           (Printf.sprintf "File error reading package %s: %s" import_path msg))
  | exn ->
      raise
        (Package_link_error
           (Printf.sprintf "Unexpected error parsing package %s: %s" import_path
              (Printexc.to_string exn)))

(** Extract package name from program AST *)
let extract_package_name (program : Ast.program) = program.package_name

(** Create qualified name for package member *)
let create_qualified_name package_name member_name =
  if package_name = "main" then member_name
  else package_name ^ "." ^ member_name

(** Extract exportable members from a package program *)
let extract_package_members (program : Ast.program) import_path =
  let package_name = extract_package_name program in
  let members = ref [] in

  (* Extract functions from program *)
  List.iter
    (fun func ->
      let qualified_name = create_qualified_name package_name func.fname in
      let member =
        { name = func.fname; qualified_name; member_type = Function func }
      in
      members := member :: !members)
    program.functions;

  (* Note: program doesn't have global_statements field in this AST version *)
  {
    package_name;
    import_path;
    source_file = "";
    (* Will be set by caller *)
    members = List.rev !members;
    ast = program;
  }

(** Load and parse a single package *)
let load_package import_path snow_mod source_file =
  try
    let file_path = resolve_import_to_file import_path snow_mod source_file in
    let program = parse_package_file file_path import_path in
    let linked_package = extract_package_members program import_path in
    { linked_package with source_file = file_path }
  with
  | Package_error msg ->
      raise
        (Package_link_error
           (Printf.sprintf "Failed to resolve package %s: %s" import_path msg))
  | exn ->
      raise
        (Package_link_error
           (Printf.sprintf "Failed to load package %s: %s" import_path
              (Printexc.to_string exn)))

(** Load all imported packages recursively from a source file *)
let rec load_imports_from_file_recursive source_file snow_mod loaded_packages =
  let imports = extract_imports_from_file source_file in
  let new_packages = ref !loaded_packages in

  List.iter
    (fun import_path ->
      if not (is_stdlib_package import_path || is_local_import import_path) then
        (* Check if package is already loaded *)
        let already_loaded =
          List.exists (fun pkg -> pkg.import_path = import_path) !new_packages
        in
        if not already_loaded then
          try
            let package = load_package import_path snow_mod source_file in
            new_packages := package :: !new_packages;
            (* Recursively load imports from this package *)
            new_packages :=
              load_imports_from_file_recursive package.source_file snow_mod
                new_packages
          with
          | Package_link_error msg -> Printf.eprintf "Warning: %s\n" msg
          | exn ->
              Printf.eprintf "Warning: Failed to load package %s: %s\n"
                import_path (Printexc.to_string exn))
    imports;

  !new_packages

(** Load all imported packages from a source file *)
let load_imports_from_file source_file snow_mod =
  let loaded_packages = ref [] in
  let final_packages =
    load_imports_from_file_recursive source_file snow_mod loaded_packages
  in
  List.rev final_packages

(** Create package context from loaded packages *)
let create_package_context loaded_packages =
  let qualified_functions = ref [] in
  let qualified_variables = ref [] in

  List.iter
    (fun package ->
      List.iter
        (fun member ->
          match member.member_type with
          | Function func ->
              qualified_functions :=
                (member.qualified_name, func) :: !qualified_functions
          | Variable (typ, expr) ->
              qualified_variables :=
                (member.qualified_name, typ, expr) :: !qualified_variables
          | Type _ ->
              (* TODO: Handle type definitions *)
              ())
        package.members)
    loaded_packages;

  {
    imported_packages = loaded_packages;
    qualified_functions = List.rev !qualified_functions;
    qualified_variables = List.rev !qualified_variables;
  }

(** Integrate package functions into main program *)
let integrate_packages_into_program main_program package_context =
  (* Add package functions to main program's function list *)
  (* Update function names to use qualified names *)
  let package_functions =
    List.map
      (fun (qualified_name, func) -> { func with fname = qualified_name })
      package_context.qualified_functions
  in
  let integrated_functions = main_program.functions @ package_functions in

  (* Note: program doesn't have global_statements field in this AST version *)
  (* Just integrate functions for now *)
  { main_program with functions = integrated_functions }

(** Resolve qualified function calls in expressions *)
let rec resolve_qualified_calls expr package_context =
  match expr with
  | EVar function_name -> (
      (* Check if this is an unqualified function name that might need to be resolved *)
      (* Look for package functions that end with this function name *)
      let matching_qualified =
        List.filter
          (fun (qualified_name, _) ->
            String.ends_with ~suffix:("." ^ function_name) qualified_name
            || qualified_name = function_name)
          package_context.qualified_functions
      in
      match matching_qualified with
      | [ (qualified_name, _) ] -> EVar qualified_name
      | [] -> expr (* No match found, leave as is *)
      | _ -> expr (* Multiple matches, leave as is to avoid ambiguity *))
  | EFieldAccess (EVar package_name, function_name) ->
      let qualified_name = package_name ^ "." ^ function_name in
      (* Check if this is a qualified function call *)
      if List.mem_assoc qualified_name package_context.qualified_functions then
        (* Transform into direct function identifier *)
        EVar qualified_name
      else expr
  | ECall (func_expr, args) ->
      let resolved_func = resolve_qualified_calls func_expr package_context in
      let resolved_args =
        List.map (fun arg -> resolve_qualified_calls arg package_context) args
      in
      ECall (resolved_func, resolved_args)
  | EBinOp (op, left, right) ->
      EBinOp
        ( op,
          resolve_qualified_calls left package_context,
          resolve_qualified_calls right package_context )
  | EUnOp (op, operand) ->
      EUnOp (op, resolve_qualified_calls operand package_context)
  | EIf (cond, then_expr, else_expr) ->
      EIf
        ( resolve_qualified_calls cond package_context,
          resolve_qualified_calls then_expr package_context,
          resolve_qualified_calls else_expr package_context )
  | ELet (name, init, body) ->
      ELet
        ( name,
          resolve_qualified_calls init package_context,
          resolve_qualified_calls body package_context )
  | EMatch (scrutinee, cases) ->
      let resolved_scrutinee =
        resolve_qualified_calls scrutinee package_context
      in
      let resolved_cases =
        List.map
          (fun (case_arm : Ast.match_arm) ->
            let resolved_body =
              resolve_qualified_calls case_arm.body package_context
            in
            let resolved_guard =
              Option.map
                (fun g -> resolve_qualified_calls g package_context)
                case_arm.guard
            in
            { case_arm with body = resolved_body; guard = resolved_guard })
          cases
      in
      EMatch (resolved_scrutinee, resolved_cases)
  | EFieldAccess (obj, field) ->
      EFieldAccess (resolve_qualified_calls obj package_context, field)
  | EArrayAccess (arr, idx) ->
      EArrayAccess
        ( resolve_qualified_calls arr package_context,
          resolve_qualified_calls idx package_context )
  | EArray exprs ->
      EArray
        (List.map (fun e -> resolve_qualified_calls e package_context) exprs)
  | ETuple exprs ->
      ETuple
        (List.map (fun e -> resolve_qualified_calls e package_context) exprs)
  | EStruct fields ->
      EStruct
        (List.map
           (fun field ->
             {
               field with
               init_value =
                 resolve_qualified_calls field.init_value package_context;
             })
           fields)
  | EUnsafe expr -> EUnsafe (resolve_qualified_calls expr package_context)
  | EBlock exprs ->
      EBlock
        (List.map (fun e -> resolve_qualified_calls e package_context) exprs)
  | _ ->
      (* For literals and simple identifiers, no transformation needed *)
      expr

(** Resolve qualified calls in statements *)
let rec resolve_qualified_calls_in_stmt stmt package_context =
  match stmt with
  | SExpr expr -> SExpr (resolve_qualified_calls expr package_context)
  | SVarDecl (name, typ, Some expr) ->
      SVarDecl (name, typ, Some (resolve_qualified_calls expr package_context))
  | SVarDecl (name, typ, None) -> SVarDecl (name, typ, None)
  | SAssign (target, expr) ->
      SAssign (target, resolve_qualified_calls expr package_context)
  | SIf (cond, then_stmts, else_stmts) ->
      SIf
        ( resolve_qualified_calls cond package_context,
          List.map
            (fun s -> resolve_qualified_calls_in_stmt s package_context)
            then_stmts,
          Option.map
            (List.map (fun s ->
                 resolve_qualified_calls_in_stmt s package_context))
            else_stmts )
  | SWhile (cond, body) ->
      SWhile
        ( resolve_qualified_calls cond package_context,
          List.map
            (fun s -> resolve_qualified_calls_in_stmt s package_context)
            body )
  | SReturn expr -> SReturn (resolve_qualified_calls expr package_context)

(** Resolve qualified calls in function definitions *)
let resolve_qualified_calls_in_function func package_context =
  let resolved_body =
    List.map
      (fun s -> resolve_qualified_calls_in_stmt s package_context)
      func.body
  in
  { func with body = resolved_body }

(** Main function to link packages with main program *)
let link_packages_with_program main_program source_file snow_mod =
  try
    (* Load all imported packages *)
    let loaded_packages = load_imports_from_file source_file snow_mod in
    let package_context = create_package_context loaded_packages in

    (* Integrate package definitions into main program *)
    let integrated_program =
      integrate_packages_into_program main_program package_context
    in

    (* Resolve qualified function calls in main program *)
    let resolved_functions =
      List.map
        (fun func -> resolve_qualified_calls_in_function func package_context)
        integrated_program.functions
    in

    let final_program =
      { integrated_program with functions = resolved_functions }
    in

    (final_program, package_context)
  with
  | Package_link_error _ as e -> raise e
  | exn ->
      raise
        (Package_link_error
           (Printf.sprintf "Failed to link packages: %s"
              (Printexc.to_string exn)))
