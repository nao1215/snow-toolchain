(*** @project snow-toolchain @module Advanced_Typechecker

  Advanced type checker with Hindley-Milner inference for SNOW language.

  This module extends the basic type checker with: - Complete type inference
  without annotations - Let-polymorphism support - Automatic Option/Result type
  resolution - Better error messages with type variable information

  Features: - Infers types of ENone and EOk expressions - Handles polymorphic
  functions properly - Provides detailed unification error messages - Supports
  complex nested type structures *)

open Ast
open Ast_utils
open Inference

exception Advanced_Type_error of string

(** Infer type of literal expressions *)
let infer_literal expr =
  try infer_literal_type expr
  with Ast_util_error msg -> raise (Advanced_Type_error msg)

(** Check if types are compatible (with unification) *)
let check_types_compatible t1 t2 =
  try
    let subst = unify t1 t2 in
    Some subst
  with Inference_error _ -> None

(** Type check binary operations with inference *)
let check_binop op t1 t2 =
  match op with
  | Add -> (
      (* Try numeric types first *)
      match check_types_compatible t1 t2 with
      | Some subst when TypeUtils.is_numeric_type (apply_subst subst t1) ->
          apply_subst subst t1
      | _ -> (
          (* Try string concatenation *)
          try
            let subst1 = unify t1 TString in
            let subst2 = unify t2 TString in
            let final_subst = compose_subst subst1 subst2 in
            apply_subst final_subst TString
          with Inference_error _ ->
            raise
              (Advanced_Type_error
                 (Printf.sprintf "Cannot add %s and %s"
                    (TypeUtils.typ_to_string t1)
                    (TypeUtils.typ_to_string t2)))))
  | Sub | Mul | Div -> (
      match check_types_compatible t1 t2 with
      | Some subst when TypeUtils.is_numeric_type (apply_subst subst t1) ->
          apply_subst subst t1
      | _ ->
          raise
            (Advanced_Type_error
               (Printf.sprintf
                  "Arithmetic operation requires numeric types, got %s and %s"
                  (TypeUtils.typ_to_string t1)
                  (TypeUtils.typ_to_string t2))))
  | Mod -> (
      match check_types_compatible t1 t2 with
      | Some subst when TypeUtils.is_integer_type (apply_subst subst t1) ->
          apply_subst subst t1
      | _ ->
          raise
            (Advanced_Type_error
               (Printf.sprintf
                  "Modulo operation requires integer types, got %s and %s"
                  (TypeUtils.typ_to_string t1)
                  (TypeUtils.typ_to_string t2))))
  | Eq | Neq -> (
      match check_types_compatible t1 t2 with
      | Some _ -> TBool
      | None ->
          raise
            (Advanced_Type_error
               (Printf.sprintf "Cannot compare %s and %s"
                  (TypeUtils.typ_to_string t1)
                  (TypeUtils.typ_to_string t2))))
  | Lt | Le | Gt | Ge -> (
      match check_types_compatible t1 t2 with
      | Some subst when TypeUtils.is_numeric_type (apply_subst subst t1) ->
          TBool
      | _ ->
          raise
            (Advanced_Type_error
               (Printf.sprintf
                  "Comparison requires numeric types, got %s and %s"
                  (TypeUtils.typ_to_string t1)
                  (TypeUtils.typ_to_string t2))))
  | And | Or -> (
      try
        let subst1 = unify t1 TBool in
        let subst2 = unify t2 TBool in
        let _ = compose_subst subst1 subst2 in
        TBool
      with Inference_error _ ->
        raise
          (Advanced_Type_error
             (Printf.sprintf
                "Logical operation requires boolean types, got %s and %s"
                (TypeUtils.typ_to_string t1)
                (TypeUtils.typ_to_string t2))))
  | _ -> raise (Advanced_Type_error "Unsupported binary operation")

(** Advanced type inference with Hindley-Milner *)
let rec infer_type env = function
  | ( EI8 _ | EU8 _ | EI16 _ | EU16 _ | EI32 _ | EU32 _ | EI64 _ | EU64 _
    | EInt _ | EUint _ | EF32 _ | EF64 _ | EFloat _ | EBool _ | EChar _
    | EString _ | EBytes _ | EUnit ) as lit ->
      (infer_literal lit, [])
  | EVar name -> (
      match lookup_env env name with
      | Some scheme -> (instantiate scheme, [])
      | None ->
          raise
            (Advanced_Type_error (Printf.sprintf "Undefined variable: %s" name))
      )
  | EBinOp (op, e1, e2) ->
      let t1, s1 = infer_type env e1 in
      let env1 =
        List.map
          (fun (n, scheme) ->
            (n, { scheme with scheme_type = apply_subst s1 scheme.scheme_type }))
          env
      in
      let t2, s2 = infer_type env1 e2 in
      let t1' = apply_subst s2 t1 in
      let result_type = check_binop op t1' t2 in
      (result_type, compose_subst s1 s2)
  | ELet (name, value, body) ->
      let value_type, s1 = infer_type env value in
      let env_free = env_free_vars env in
      let scheme = generalize env_free value_type in
      let new_env = extend_env env name scheme in
      let body_type, s2 = infer_type new_env body in
      (body_type, compose_subst s1 s2)
  | EIf (cond, then_expr, else_expr) ->
      let cond_type, s1 = infer_type env cond in
      let s_bool =
        try unify cond_type TBool
        with Inference_error _ ->
          raise (Advanced_Type_error "If condition must be boolean")
      in
      let s1' = compose_subst s1 s_bool in
      let env1 =
        List.map
          (fun (n, scheme) ->
            (n, { scheme with scheme_type = apply_subst s1' scheme.scheme_type }))
          env
      in

      let then_type, s2 = infer_type env1 then_expr in
      let env2 =
        List.map
          (fun (n, scheme) ->
            (n, { scheme with scheme_type = apply_subst s2 scheme.scheme_type }))
          env1
      in
      let else_type, s3 = infer_type env2 else_expr in

      let then_type' = apply_subst s3 then_type in
      let s_branch =
        try unify then_type' else_type
        with Inference_error _ ->
          raise
            (Advanced_Type_error
               (Printf.sprintf "If branches have incompatible types: %s vs %s"
                  (TypeUtils.typ_to_string then_type')
                  (TypeUtils.typ_to_string else_type)))
      in

      ( apply_subst s_branch else_type,
        compose_subst (compose_subst s1' s2) (compose_subst s3 s_branch) )
  | ESome expr ->
      let inner_type, subst = infer_type env expr in
      (TOption inner_type, subst)
  | ENone ->
      (* ENone can now be inferred as Option<'a> where 'a is a fresh type
         variable *)
      let fresh_var = fresh_type_var () in
      (TOption fresh_var, [])
  | EOk expr ->
      let ok_type, subst = infer_type env expr in
      (* Create Result<T, 'e> where 'e is a fresh error type variable *)
      let err_var = fresh_type_var () in
      (TResult (ok_type, err_var), subst)
  | EErr expr ->
      let err_type, subst = infer_type env expr in
      (* Create Result<'t, E> where 't is a fresh success type variable *)
      let ok_var = fresh_type_var () in
      (TResult (ok_var, err_type), subst)
  | EPrint expr ->
      (* print accepts any printable type and returns unit *)
      let _, subst = infer_type env expr in
      (TUnit, subst)
  | EPrintln expr ->
      (* println accepts any printable type and returns unit *)
      let _, subst = infer_type env expr in
      (TUnit, subst)
  | ETuple exprs ->
      let rec infer_tuple_types acc_types acc_subst = function
        | [] -> (List.rev acc_types, acc_subst)
        | expr :: rest ->
            let env' =
              List.map
                (fun (n, scheme) ->
                  ( n,
                    {
                      scheme with
                      scheme_type = apply_subst acc_subst scheme.scheme_type;
                    } ))
                env
            in
            let expr_type, expr_subst = infer_type env' expr in
            let new_subst = compose_subst acc_subst expr_subst in
            infer_tuple_types (expr_type :: acc_types) new_subst rest
      in
      let types, subst = infer_tuple_types [] [] exprs in
      (TTuple types, subst)
  | EArray exprs -> (
      match exprs with
      | [] ->
          (* Empty array: infer as [T] where T is fresh type variable *)
          let elem_var = fresh_type_var () in
          (TArray (elem_var, Some 0), [])
      | first :: rest ->
          let first_type, s1 = infer_type env first in
          let rec check_elements acc_subst elem_type = function
            | [] -> (elem_type, acc_subst)
            | expr :: remaining ->
                let env' =
                  List.map
                    (fun (n, scheme) ->
                      ( n,
                        {
                          scheme with
                          scheme_type = apply_subst acc_subst scheme.scheme_type;
                        } ))
                    env
                in
                let expr_type, expr_subst = infer_type env' expr in
                let unify_subst =
                  try unify elem_type expr_type
                  with Inference_error msg ->
                    raise
                      (Advanced_Type_error
                         (Printf.sprintf
                            "Array elements have incompatible types: %s" msg))
                in
                let new_subst =
                  compose_subst (compose_subst acc_subst expr_subst) unify_subst
                in
                check_elements new_subst
                  (apply_subst unify_subst elem_type)
                  remaining
          in
          let final_elem_type, final_subst =
            check_elements s1 first_type rest
          in
          (TArray (final_elem_type, Some (List.length exprs)), final_subst))
  | EFun (params, ret_type_ann, body) ->
      (* Create type variables for parameters if not annotated *)
      let param_types =
        List.map
          (fun param ->
            match param.param_type with
            | TVar _ -> fresh_type_var () (* Use fresh var if not specified *)
            | concrete_type -> concrete_type)
          params
      in

      (* Extend environment with parameter bindings *)
      let param_schemes = List.map mono_scheme param_types in
      let param_bindings =
        List.combine (List.map (fun p -> p.name) params) param_schemes
      in
      let extended_env =
        List.fold_left
          (fun env (name, scheme) -> extend_env env name scheme)
          env param_bindings
      in

      (* Infer body type *)
      let body_type, body_subst = infer_type extended_env body in

      (* Handle return type annotation *)
      let final_ret_type =
        match ret_type_ann with
        | TVar _ -> body_type (* No annotation, use inferred type *)
        | concrete_ret -> (
            (* Check that inferred body type matches annotation *)
            try
              let ret_subst = unify body_type concrete_ret in
              apply_subst ret_subst concrete_ret
            with Inference_error _ ->
              raise
                (Advanced_Type_error
                   (Printf.sprintf
                      "Function body type %s doesn't match return annotation %s"
                      (TypeUtils.typ_to_string body_type)
                      (TypeUtils.typ_to_string concrete_ret))))
      in

      let final_param_types = List.map (apply_subst body_subst) param_types in
      (TFunc (final_param_types, final_ret_type), body_subst)
  | _ ->
      raise
        (Advanced_Type_error
           "Expression not supported in advanced type inference yet")

(** Main type inference entry point *)
let infer_expression_type expr =
  reset_type_vars ();
  try
    let inferred_type, final_subst = infer_type empty_env expr in
    Ok (apply_subst final_subst inferred_type)
  with
  | Advanced_Type_error msg -> Error msg
  | Inference_error msg -> Error ("Type inference failed: " ^ msg)
