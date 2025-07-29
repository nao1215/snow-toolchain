(*** @project snow-toolchain @module Semantic

  Semantic analyzer for the SNOW programming language.

  This module performs semantic analysis including: - Variable scope management
  - Naming convention validation - Undefined variable detection

  Design principles: - Small incremental implementation - Clear separation of
  concerns - Test-driven development *)

open Ast

exception Semantic_error of string

(** Scope entry for tracking bindings *)
type binding = Var of string * typ

type context = { scopes : binding list list (* Stack of scopes *) }
(** Semantic analysis context *)

(** Empty context *)
let empty_context = { scopes = [ [] ] }

(** Push new scope *)
let push_scope ctx = { scopes = [] :: ctx.scopes }

(** Pop scope *)
let pop_scope ctx =
  match ctx.scopes with
  | [] -> raise (Semantic_error "Cannot pop empty scope stack")
  | _ :: rest -> { scopes = rest }

(** Add variable to current scope *)
let add_var ctx name typ =
  match ctx.scopes with
  | [] -> raise (Semantic_error "No current scope")
  | current :: rest -> { scopes = (Var (name, typ) :: current) :: rest }

(** Check if variable is defined *)
let is_var_defined ctx name =
  let rec check_scopes = function
    | [] -> false
    | scope :: rest ->
        if List.exists (function Var (n, _) -> n = name) scope then true
        else check_scopes rest
  in
  check_scopes ctx.scopes

(** Validate variable naming convention *)
let validate_var_name name =
  match String.get name 0 with
  | 'a' .. 'z' | '_' -> ()
  | _ ->
      raise
        (Semantic_error
           (Printf.sprintf
              "Variable '%s' should start with lowercase letter or underscore"
              name))

(** Analyze expression *)
let rec analyze_expr ctx = function
  | EVar name ->
      if not (is_var_defined ctx name) then
        raise (Semantic_error (Printf.sprintf "Undefined variable: %s" name));
      ctx
  | ELet (name, value, body) ->
      validate_var_name name;
      let ctx = analyze_expr ctx value in
      let ctx = add_var ctx name TUnit in
      (* Type will come from type checker *)
      analyze_expr ctx body
  | EBinOp (_, e1, e2) ->
      let ctx = analyze_expr ctx e1 in
      analyze_expr ctx e2
  | EIf (cond, then_expr, else_expr) ->
      let ctx = analyze_expr ctx cond in
      let ctx = analyze_expr ctx then_expr in
      analyze_expr ctx else_expr
  | ETuple exprs -> List.fold_left analyze_expr ctx exprs
  | EArray exprs -> List.fold_left analyze_expr ctx exprs
  | ESome expr -> analyze_expr ctx expr
  | ENone -> ctx (* None has no value, no analysis needed *)
  | EOk expr -> analyze_expr ctx expr
  | EErr expr -> analyze_expr ctx expr
  | EMatch (scrutinee, arms) ->
      let ctx = analyze_expr ctx scrutinee in
      List.fold_left analyze_match_arm ctx arms
  | EFun (params, _, body) ->
      let fun_ctx = push_scope ctx in
      let fun_ctx =
        List.fold_left
          (fun ctx param ->
            validate_var_name param.name;
            add_var ctx param.name param.param_type)
          fun_ctx params
      in
      let _ = analyze_expr fun_ctx body in
      ctx (* Function parameters are local to function body *)
  | ECall (func_expr, args) ->
      let ctx = analyze_expr ctx func_expr in
      List.fold_left analyze_expr ctx args
  | EBlock exprs -> List.fold_left analyze_expr ctx exprs
  | EWhile (condition, body) ->
      let ctx = analyze_expr ctx condition in
      analyze_expr ctx body
  | EFor (var_name, iterable, body) ->
      validate_var_name var_name;
      let ctx = analyze_expr ctx iterable in
      let loop_ctx = push_scope ctx in
      let loop_ctx = add_var loop_ctx var_name TUnit in
      (* Type determined by type checker *)
      let _ = analyze_expr loop_ctx body in
      ctx (* Loop variable is local to loop body *)
  (* Literals don't need analysis *)
  | EI8 _ | EU8 _ | EI16 _ | EU16 _ | EI32 _ | EU32 _ | EI64 _ | EU64 _ | EInt _
  | EUint _ | EF32 _ | EF64 _ | EFloat _ | EBool _ | EChar _ | EString _
  | EBytes _ | EUnit ->
      ctx
  | EUnOp (_, expr) -> analyze_expr ctx expr
  | EArrayAccess (array_expr, index_expr) ->
      let ctx = analyze_expr ctx array_expr in
      analyze_expr ctx index_expr
  | EFieldAccess (struct_expr, _field_name) -> analyze_expr ctx struct_expr
  | EStruct field_inits ->
      List.fold_left
        (fun ctx init -> analyze_expr ctx init.init_value)
        ctx field_inits
  | EPrint expr -> analyze_expr ctx expr
  | EPrintln expr -> analyze_expr ctx expr
  | EUnsafe expr -> analyze_expr ctx expr

(** Analyze pattern for variable bindings *)
and analyze_pattern ctx = function
  | PWildcard | PNone -> ctx
  | PVar name ->
      validate_var_name name;
      add_var ctx name TUnit (* Type will be determined by type checker *)
  | PLiteral _ -> ctx (* Literals don't introduce bindings *)
  | PSome inner_pattern -> analyze_pattern ctx inner_pattern
  | POk inner_pattern -> analyze_pattern ctx inner_pattern
  | PErr inner_pattern -> analyze_pattern ctx inner_pattern
  | PTuple patterns -> List.fold_left analyze_pattern ctx patterns
  | PArray patterns -> List.fold_left analyze_pattern ctx patterns
  | PRange (min_expr, max_expr) ->
      let _ = analyze_expr ctx min_expr in
      let _ = analyze_expr ctx max_expr in
      ctx
  | PStruct field_patterns ->
      List.fold_left
        (fun acc_ctx (_field_name, pattern) -> analyze_pattern acc_ctx pattern)
        ctx field_patterns

(** Analyze a match arm *)
and analyze_match_arm ctx arm =
  let pattern_ctx = push_scope ctx in
  let pattern_ctx = analyze_pattern pattern_ctx arm.pattern in
  let _ = analyze_expr pattern_ctx arm.body in
  ctx (* Pattern bindings are local to the arm *)

(** Semantic check an expression *)
let semantic_check expr =
  try
    let _ = analyze_expr empty_context expr in
    Ok ()
  with Semantic_error msg -> Error msg
