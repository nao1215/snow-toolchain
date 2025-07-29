(*** @project snow-toolchain @module Typechecker

  Type checker for the SNOW programming language.

  This module implements basic type checking and inference for SNOW expressions.
  Starting with a minimal implementation that handles: - Literal expressions -
  Binary operations on numeric types - Variables and let bindings

  Design principles: - Small incremental steps - Clear error messages -
  Test-driven development *)

open Ast
open Ast_utils

exception Type_error of string

type type_env = (string * typ) list
(** Type environment for variable bindings *)

(** Empty type environment *)
let empty_env = []

(** Add variable to environment *)
let extend_env env name typ = (name, typ) :: env

(** Look up variable in environment *)
let lookup_env env name =
  try Some (List.assoc name env) with Not_found -> None

(** Infer type of literal expressions *)
let infer_literal expr =
  try infer_literal_type expr
  with Ast_util_error msg -> raise (Type_error msg)

(** Check if types are equal *)
let types_equal t1 t2 = types_equal t1 t2

(** Check if type is numeric *)
let is_numeric t =
  match t with
  | TI8 | TU8 | TI16 | TU16 | TI32 | TU32 | TI64 | TU64 | TInt | TUint | TF32
  | TF64 | TFloat ->
      true
  | _ -> false

(** Type check binary operations *)
let check_binop op t1 t2 =
  match op with
  | Add when is_numeric t1 && types_equal t1 t2 -> t1
  | Add when types_equal t1 TString && types_equal t2 TString -> TString
  | (Sub | Mul | Div) when is_numeric t1 && types_equal t1 t2 -> t1
  | Mod when TypeUtils.is_integer_type t1 && types_equal t1 t2 -> t1
  | (Eq | Neq) when types_equal t1 t2 -> TBool
  | (Lt | Le | Gt | Ge) when is_numeric t1 && types_equal t1 t2 -> TBool
  | (And | Or) when types_equal t1 TBool && types_equal t2 TBool -> TBool
  | _ ->
      let op_str =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "&&"
        | Or -> "||"
        | Assign -> ":="
        | Pipe -> "|>"
      in
      raise
        (Type_error
           (Printf.sprintf "Type error in %s: %s and %s" op_str
              (TypeUtils.typ_to_string t1)
              (TypeUtils.typ_to_string t2)))

(** Type check pattern against expected type and return bindings *)
let rec check_pattern_type expected_type = function
  | PWildcard -> []
  | PVar name -> [ (name, expected_type) ]
  | PLiteral lit ->
      let lit_type = infer_literal lit in
      if not (types_equal lit_type expected_type) then
        raise
          (Type_error
             (Printf.sprintf "Pattern literal type %s doesn't match expected %s"
                (TypeUtils.typ_to_string lit_type)
                (TypeUtils.typ_to_string expected_type)));
      []
  | PSome inner_pattern -> (
      match expected_type with
      | TOption inner_type -> check_pattern_type inner_type inner_pattern
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Some pattern expects Option type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PNone -> (
      match expected_type with
      | TOption _ -> []
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "None pattern expects Option type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | POk inner_pattern -> (
      match expected_type with
      | TResult (ok_type, _) -> check_pattern_type ok_type inner_pattern
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Ok pattern expects Result type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PErr inner_pattern -> (
      match expected_type with
      | TResult (_, err_type) -> check_pattern_type err_type inner_pattern
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Err pattern expects Result type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PTuple patterns -> (
      match expected_type with
      | TTuple types when List.length patterns = List.length types ->
          List.fold_left2
            (fun acc pattern typ -> acc @ check_pattern_type typ pattern)
            [] patterns types
      | TTuple types ->
          raise
            (Type_error
               (Printf.sprintf
                  "Tuple pattern length mismatch: expected %d, got %d"
                  (List.length types) (List.length patterns)))
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Tuple pattern expects tuple type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PArray patterns -> (
      match expected_type with
      | TArray (elem_type, Some size) when List.length patterns = size ->
          List.fold_left
            (fun acc pattern -> acc @ check_pattern_type elem_type pattern)
            [] patterns
      | TArray (elem_type, None) ->
          (* Dynamic array - accept any number of patterns *)
          List.fold_left
            (fun acc pattern -> acc @ check_pattern_type elem_type pattern)
            [] patterns
      | TArray (_, Some size) ->
          raise
            (Type_error
               (Printf.sprintf
                  "Array pattern length mismatch: expected %d, got %d" size
                  (List.length patterns)))
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Array pattern expects array type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PRange (min_expr, max_expr) -> (
      (* Range patterns are only valid for numeric types *)
      match expected_type with
      | TI8 | TU8 | TI16 | TU16 | TI32 | TU32 | TI64 | TU64 | TInt | TUint
      | TF32 | TF64 | TFloat ->
          let _ = (min_expr, max_expr) in
          [] (* Type checking of range bounds would be done elsewhere *)
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Range pattern expects numeric type, got %s"
                  (TypeUtils.typ_to_string expected_type))))
  | PStruct field_patterns -> (
      match expected_type with
      | TStruct expected_fields ->
          List.fold_left
            (fun acc (field_name, pattern) ->
              (* Find field type in expected struct *)
              let field_type =
                List.find_map
                  (fun field ->
                    if field.field_name = field_name then Some field.field_type
                    else None)
                  expected_fields
              in
              match field_type with
              | Some typ -> acc @ check_pattern_type typ pattern
              | None ->
                  raise
                    (Type_error
                       (Printf.sprintf "Unknown field %s in struct pattern"
                          field_name)))
            [] field_patterns
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Struct pattern expects struct type, got %s"
                  (TypeUtils.typ_to_string expected_type))))

(** Main type inference function *)
let rec infer_type env = function
  | ( EI8 _ | EU8 _ | EI16 _ | EU16 _ | EI32 _ | EU32 _ | EI64 _ | EU64 _
    | EInt _ | EUint _ | EF32 _ | EF64 _ | EFloat _ | EBool _ | EChar _
    | EString _ | EBytes _ | EUnit ) as lit ->
      infer_literal lit
  | EVar name -> (
      match lookup_env env name with
      | Some typ -> typ
      | None -> (
          (* Check for built-in functions *)
          match name with
          | "print" -> TFunc ([ TString ], TUnit)
          | "println" -> TFunc ([ TString ], TUnit)
          | "ptr_to_i64" ->
              TFunc ([ TString ], TI64)
              (* Temporary: use TString as generic pointer *)
          | "i64_to_ptr" ->
              TFunc ([ TI64 ], TString)
              (* Temporary: use TString as generic pointer *)
          | "malloc" ->
              TFunc ([ TI64 ], TString)
              (* Temporary: use TString as generic pointer *)
          | "free" ->
              TFunc ([ TString ], TUnit)
              (* Temporary: use TString as generic pointer *)
          | _ ->
              raise (Type_error (Printf.sprintf "Undefined variable: %s" name)))
      )
  | EBinOp (op, e1, e2) ->
      let t1 = infer_type env e1 in
      let t2 = infer_type env e2 in
      check_binop op t1 t2
  | ELet (name, value, body) ->
      let value_type = infer_type env value in
      let new_env = extend_env env name value_type in
      infer_type new_env body
  | EIf (cond, then_expr, else_expr) ->
      let cond_type = infer_type env cond in
      if not (types_equal cond_type TBool) then
        raise
          (Type_error
             (Printf.sprintf "If condition must be bool, got %s"
                (TypeUtils.typ_to_string cond_type)));
      let then_type = infer_type env then_expr in
      let else_type = infer_type env else_expr in
      if not (types_equal then_type else_type) then
        raise
          (Type_error
             (Printf.sprintf "If branches have different types: %s vs %s"
                (TypeUtils.typ_to_string then_type)
                (TypeUtils.typ_to_string else_type)));
      then_type
  | ESome expr ->
      let inner_type = infer_type env expr in
      TOption inner_type
  | ENone ->
      (* ENone cannot be type-inferred without context, requires type
         annotation *)
      raise
        (Type_error "None requires type annotation (type inference limitation)")
  | EOk expr ->
      let _ = infer_type env expr in
      (* Result<T, E> error type E cannot be inferred, requires type
         annotation *)
      raise
        (Type_error
           "Ok requires error type annotation (type inference limitation)")
  | EErr expr ->
      let _ = infer_type env expr in
      (* Result<T, E> value type T cannot be inferred, requires type
         annotation *)
      raise
        (Type_error
           "Err requires value type annotation (type inference limitation)")
  | ETuple exprs ->
      let types = List.map (infer_type env) exprs in
      TTuple types
  | EArray exprs -> (
      match exprs with
      | [] ->
          (* Empty array cannot be type-inferred, requires type annotation *)
          raise (Type_error "Empty array requires type annotation")
      | first :: rest ->
          let first_type = infer_type env first in
          List.iter
            (fun expr ->
              let expr_type = infer_type env expr in
              if not (types_equal first_type expr_type) then
                raise
                  (Type_error
                     (Printf.sprintf
                        "Array elements must have same type: expected %s, got \
                         %s"
                        (TypeUtils.typ_to_string first_type)
                        (TypeUtils.typ_to_string expr_type))))
            rest;
          TArray (first_type, None)
          (* Treat as variable-length array *))
  | EMatch (scrutinee, arms) -> (
      let scrutinee_type = infer_type env scrutinee in
      match arms with
      | [] -> raise (Type_error "Match expression requires at least one arm")
      | first_arm :: rest_arms ->
          (* Pattern matching type checking is complex, providing basic
             implementation *)
          let first_arm_type = infer_match_arm env scrutinee_type first_arm in
          List.iter
            (fun arm ->
              let arm_type = infer_match_arm env scrutinee_type arm in
              if not (types_equal first_arm_type arm_type) then
                raise
                  (Type_error
                     (Printf.sprintf "Match arms have different types: %s vs %s"
                        (TypeUtils.typ_to_string first_arm_type)
                        (TypeUtils.typ_to_string arm_type))))
            rest_arms;
          first_arm_type)
  | EFun (params, return_type, body) ->
      let param_env =
        List.fold_left
          (fun env param -> extend_env env param.name param.param_type)
          env params
      in
      let body_type = infer_type param_env body in
      if not (types_equal body_type return_type) then
        raise
          (Type_error
             (Printf.sprintf
                "Function body type %s doesn't match declared return type %s"
                (TypeUtils.typ_to_string body_type)
                (TypeUtils.typ_to_string return_type)));
      let param_types = List.map (fun p -> p.param_type) params in
      TFunc (param_types, return_type)
  | ECall (func_expr, args) -> (
      let func_type = infer_type env func_expr in
      match func_type with
      | TFunc (param_types, return_type) ->
          if List.length args != List.length param_types then
            raise
              (Type_error
                 (Printf.sprintf "Function expects %d arguments, got %d"
                    (List.length param_types) (List.length args)));
          List.iter2
            (fun arg expected_type ->
              let arg_type = infer_type env arg in
              if not (types_equal arg_type expected_type) then
                raise
                  (Type_error
                     (Printf.sprintf
                        "Argument type %s doesn't match expected %s"
                        (TypeUtils.typ_to_string arg_type)
                        (TypeUtils.typ_to_string expected_type))))
            args param_types;
          return_type
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Cannot call non-function type %s"
                  (TypeUtils.typ_to_string func_type))))
  | EBlock exprs -> (
      match exprs with
      | [] -> TUnit (* Empty block returns unit *)
      | _ ->
          (* All expressions except the last are evaluated for side effects *)
          let rec check_exprs = function
            | [ last ] ->
                infer_type env last (* Last expression determines block type *)
            | expr :: rest ->
                let _ = infer_type env expr in
                (* Check but ignore type *)
                check_exprs rest
            | [] -> TUnit (* Should not happen due to match above *)
          in
          check_exprs exprs)
  | EWhile (condition, body) ->
      let cond_type = infer_type env condition in
      if not (types_equal cond_type TBool) then
        raise
          (Type_error
             (Printf.sprintf "While condition must be bool, got %s"
                (TypeUtils.typ_to_string cond_type)));
      let _ = infer_type env body in
      (* Check body but ignore type *)
      TUnit
      (* While loops return unit *)
  | EFor (var_name, iterable, body) -> (
      let iterable_type = infer_type env iterable in
      match iterable_type with
      | TArray (element_type, _) ->
          (* Add loop variable to environment for body *)
          let loop_env = extend_env env var_name element_type in
          let _ = infer_type loop_env body in
          (* Check body but ignore type *)
          TUnit
          (* For loops return unit *)
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "For loop requires array type, got %s"
                  (TypeUtils.typ_to_string iterable_type))))
  | EStruct _field_inits ->
      (* For struct literals, we need type annotation or inference from context *)
      (* This is a limitation - struct literals need explicit type annotation *)
      raise (Type_error "Struct literals require type annotation")
  | EPrint _expr -> TUnit
  | EPrintln _expr -> TUnit
  | EFieldAccess (struct_expr, field_name) -> (
      let struct_type = infer_type env struct_expr in
      match struct_type with
      | TStruct fields -> (
          try
            let field = List.find (fun f -> f.field_name = field_name) fields in
            field.field_type
          with Not_found ->
            raise
              (Type_error
                 (Printf.sprintf "Field %s not found in struct" field_name)))
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Field access on non-struct type %s"
                  (TypeUtils.typ_to_string struct_type))))
  | EArrayAccess (array_expr, index_expr) -> (
      let array_type = infer_type env array_expr in
      let index_type = infer_type env index_expr in
      if not (TypeUtils.is_integer_type index_type) then
        raise
          (Type_error
             (Printf.sprintf "Array index must be integer, got %s"
                (TypeUtils.typ_to_string index_type)));
      match array_type with
      | TArray (element_type, _) -> element_type
      | _ ->
          raise
            (Type_error
               (Printf.sprintf "Array access on non-array type %s"
                  (TypeUtils.typ_to_string array_type))))
  | EUnOp (op, expr) -> (
      let expr_type = infer_type env expr in
      match op with
      | Not ->
          if not (types_equal expr_type TBool) then
            raise
              (Type_error
                 (Printf.sprintf "Logical not requires bool, got %s"
                    (TypeUtils.typ_to_string expr_type)));
          TBool
      | Neg ->
          if not (is_numeric expr_type) then
            raise
              (Type_error
                 (Printf.sprintf "Negation requires numeric type, got %s"
                    (TypeUtils.typ_to_string expr_type)));
          expr_type)
  | EUnsafe expr ->
      (* Unsafe expressions pass through their inner expression's type *)
      infer_type env expr

(** Type check a match arm *)
and infer_match_arm env scrutinee_type arm =
  let bindings = check_pattern_type scrutinee_type arm.pattern in
  let extended_env =
    List.fold_left (fun env (name, typ) -> extend_env env name typ) env bindings
  in
  infer_type extended_env arm.body

(** Type check an expression *)
let type_check expr =
  try Ok (infer_type empty_env expr) with Type_error msg -> Error msg
