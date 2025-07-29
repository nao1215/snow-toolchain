(*** @project snow-toolchain @module Analyzer

  Combined analyzer for the SNOW programming language.

  This module integrates type checking and semantic analysis to provide complete
  static analysis of SNOW programs.

  Design principles: - Unified error reporting - Single pass when possible -
  Clear separation of concerns *)

open Ast
open Typechecker
open Semantic

type analysis_result = { expr_type : typ; warnings : string list }
(** Analysis result containing type and any warnings *)

(** Analyze an expression with both type checking and semantic analysis *)
let analyze expr =
  (* First perform semantic analysis *)
  match semantic_check expr with
  | Error msg -> Error msg
  | Ok () -> (
      (* Then perform type checking *)
      match type_check expr with
      | Error msg -> Error msg
      | Ok typ -> Ok { expr_type = typ; warnings = [] })

(** Analyze and return just the type (for compatibility) *)
let analyze_type expr =
  match analyze expr with
  | Error msg -> Error msg
  | Ok result -> Ok result.expr_type
