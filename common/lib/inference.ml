(*** @project snow-toolchain @module Inference

  Hindley-Milner type inference system for SNOW language.

  This module implements a complete type inference algorithm that can: - Infer
  types without explicit annotations - Handle polymorphic functions with
  let-polymorphism - Automatically resolve Option/Result types - Provide
  detailed error messages for type conflicts

  Core components: - Type variable generation and management - Unification
  algorithm for type constraint solving - Type scheme generalization for
  polymorphism - Substitution and type environment management *)

open Ast

exception Inference_error of string
(** Type inference exceptions *)

(** Type variable counter for generating unique type variables *)
let type_var_counter = ref 0

(** Generate a fresh type variable *)
let fresh_type_var () =
  let id = !type_var_counter in
  incr type_var_counter;
  TVar id

type substitution = (int * typ) list
(** Substitution: mapping from type variables to types *)

(** Apply substitution to a type *)
let rec apply_subst subst = function
  | TVar id -> ( try List.assoc id subst with Not_found -> TVar id)
  | TArray (elem_type, size) -> TArray (apply_subst subst elem_type, size)
  | TTuple types -> TTuple (List.map (apply_subst subst) types)
  | TOption inner_type -> TOption (apply_subst subst inner_type)
  | TResult (ok_type, err_type) ->
      TResult (apply_subst subst ok_type, apply_subst subst err_type)
  | TFunc (param_types, return_type) ->
      TFunc
        (List.map (apply_subst subst) param_types, apply_subst subst return_type)
  | TStruct fields ->
      TStruct
        (List.map
           (fun field ->
             { field with field_type = apply_subst subst field.field_type })
           fields)
  | TUnion (name, variants) ->
      TUnion
        ( name,
          List.map
            (fun variant ->
              {
                variant with
                variant_data =
                  (match variant.variant_data with
                  | Some typ -> Some (apply_subst subst typ)
                  | None -> None);
              })
            variants )
  | t -> t (* Primitive types remain unchanged *)

(** Compose two substitutions *)
let compose_subst s1 s2 =
  List.map (fun (id, t) -> (id, apply_subst s1 t)) s2 @ s1

(** Get free type variables in a type *)
let rec free_type_vars = function
  | TVar id -> [ id ]
  | TArray (elem_type, _) -> free_type_vars elem_type
  | TTuple types -> List.fold_left ( @ ) [] (List.map free_type_vars types)
  | TOption inner_type -> free_type_vars inner_type
  | TResult (ok_type, err_type) ->
      free_type_vars ok_type @ free_type_vars err_type
  | TFunc (param_types, return_type) ->
      List.fold_left ( @ ) [] (List.map free_type_vars param_types)
      @ free_type_vars return_type
  | TStruct fields ->
      List.fold_left ( @ ) []
        (List.map (fun field -> free_type_vars field.field_type) fields)
  | TUnion (_, variants) ->
      List.fold_left ( @ ) []
        (List.map
           (fun variant ->
             match variant.variant_data with
             | Some typ -> free_type_vars typ
             | None -> [])
           variants)
  | _ -> [] (* Primitive types have no free variables *)

(** Occurs check: prevent infinite types *)
let rec occurs_check id = function
  | TVar id' -> id = id'
  | TArray (elem_type, _) -> occurs_check id elem_type
  | TTuple types -> List.exists (occurs_check id) types
  | TOption inner_type -> occurs_check id inner_type
  | TResult (ok_type, err_type) ->
      occurs_check id ok_type || occurs_check id err_type
  | TFunc (param_types, return_type) ->
      List.exists (occurs_check id) param_types || occurs_check id return_type
  | TStruct fields ->
      List.exists (fun field -> occurs_check id field.field_type) fields
  | TUnion (_, variants) ->
      List.exists
        (fun variant ->
          match variant.variant_data with
          | Some typ -> occurs_check id typ
          | None -> false)
        variants
  | _ -> false

(** Unification algorithm: find substitution that makes two types equal *)
let rec unify t1 t2 =
  let t1 = apply_subst [] t1 in
  let t2 = apply_subst [] t2 in

  match (t1, t2) with
  | t1, t2 when t1 = t2 -> [] (* Same types unify with empty substitution *)
  | TVar id, t | t, TVar id ->
      if occurs_check id t then
        raise
          (Inference_error
             (Printf.sprintf
                "Occurs check failed: cannot unify type variable %d with %s" id
                (TypeUtils.typ_to_string t)))
      else [ (id, t) ]
  | TArray (elem1, size1), TArray (elem2, size2) when size1 = size2 ->
      unify elem1 elem2
  | TTuple types1, TTuple types2 when List.length types1 = List.length types2 ->
      List.fold_left2
        (fun acc t1 t2 ->
          compose_subst acc (unify (apply_subst acc t1) (apply_subst acc t2)))
        [] types1 types2
  | TOption inner1, TOption inner2 -> unify inner1 inner2
  | TResult (ok1, err1), TResult (ok2, err2) ->
      let s1 = unify ok1 ok2 in
      let s2 = unify (apply_subst s1 err1) (apply_subst s1 err2) in
      compose_subst s1 s2
  | TFunc (params1, ret1), TFunc (params2, ret2)
    when List.length params1 = List.length params2 ->
      let param_subst =
        List.fold_left2
          (fun acc t1 t2 ->
            compose_subst acc (unify (apply_subst acc t1) (apply_subst acc t2)))
          [] params1 params2
      in
      let ret_subst =
        unify (apply_subst param_subst ret1) (apply_subst param_subst ret2)
      in
      compose_subst param_subst ret_subst
  | _ ->
      raise
        (Inference_error
           (Printf.sprintf "Cannot unify %s with %s"
              (TypeUtils.typ_to_string t1)
              (TypeUtils.typ_to_string t2)))

type type_scheme = {
  quantified_vars : int list; (* Universally quantified type variables *)
  scheme_type : typ; (* The actual type *)
}
(** Type scheme for polymorphism *)

(** Create monomorphic type scheme *)
let mono_scheme t = { quantified_vars = []; scheme_type = t }

(** Generalize a type: bind free type variables *)
let generalize env_free_vars typ =
  let type_free_vars = free_type_vars typ in
  let quantified =
    List.filter (fun id -> not (List.mem id env_free_vars)) type_free_vars
  in
  { quantified_vars = quantified; scheme_type = typ }

(** Instantiate a type scheme with fresh type variables *)
let instantiate scheme =
  let fresh_vars =
    List.map (fun _ -> fresh_type_var ()) scheme.quantified_vars
  in
  let subst = List.combine scheme.quantified_vars fresh_vars in
  apply_subst subst scheme.scheme_type

type type_env = (string * type_scheme) list
(** Type environment with type schemes *)

(** Empty type environment *)
let empty_env = []

(** Add binding to environment *)
let extend_env env name scheme = (name, scheme) :: env

(** Look up variable in environment *)
let lookup_env env name =
  try Some (List.assoc name env) with Not_found -> None

(** Get free type variables in environment *)
let env_free_vars env =
  List.fold_left ( @ ) []
    (List.map (fun (_, scheme) -> free_type_vars scheme.scheme_type) env)

(** Reset type variable counter for testing *)
let reset_type_vars () = type_var_counter := 0
