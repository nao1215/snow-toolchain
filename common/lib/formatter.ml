(** SNOW Code Formatter

    This module implements a code formatter for the SNOW programming language,
    similar to go fmt. It takes parsed AST and pretty-prints it with consistent
    formatting rules.

    Formatting Rules:
    - Consistent indentation (2 spaces)
    - Proper spacing around operators
    - Consistent line breaks
    - Standardized function/type definitions *)

open Ast

type fmt_context = {
  indent_level : int;
  indent_string : string; (* Usually 2 spaces *)
}
(** Formatting context *)

(** Create a new formatting context *)
let create_context () = { indent_level = 0; indent_string = "\t" }

(** Get current indentation string *)
let indent ctx =
  String.concat "" (List.init ctx.indent_level (fun _ -> ctx.indent_string))

(** Increase indentation level *)
let indent_more ctx = { ctx with indent_level = ctx.indent_level + 1 }

(** Decrease indentation level *)
let indent_less ctx = { ctx with indent_level = max 0 (ctx.indent_level - 1) }

(** Format a type *)
let rec format_type = function
  | TVar id -> "'t" ^ string_of_int id
  | TI8 -> "i8"
  | TU8 -> "u8"
  | TI16 -> "i16"
  | TU16 -> "u16"
  | TI32 -> "i32"
  | TU32 -> "u32"
  | TI64 -> "i64"
  | TU64 -> "u64"
  | TInt -> "int"
  | TUint -> "uint"
  | TF32 -> "f32"
  | TF64 -> "f64"
  | TFloat -> "float"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TBytes -> "bytes"
  | TUnit -> "unit"
  | TNever -> "never"
  | TArray (elem_type, size) ->
      let size_str = match size with Some s -> string_of_int s | None -> "" in
      Printf.sprintf "[%s; %s]" (format_type elem_type) size_str
  | TTuple types -> "(" ^ String.concat ", " (List.map format_type types) ^ ")"
  | TOption typ -> Printf.sprintf "Option<%s>" (format_type typ)
  | TResult (ok_type, err_type) ->
      Printf.sprintf "Result<%s, %s>" (format_type ok_type)
        (format_type err_type)
  | TFunc (param_types, return_type) ->
      let params = String.concat ", " (List.map format_type param_types) in
      Printf.sprintf "(%s) -> %s" params (format_type return_type)
  | TStruct fields ->
      let format_field f =
        Printf.sprintf "%s: %s" f.field_name (format_type f.field_type)
      in
      "struct { " ^ String.concat ", " (List.map format_field fields) ^ " }"
  | TUnion (name, variants) ->
      let format_variant v =
        match v.variant_data with
        | None -> v.variant_name
        | Some typ -> Printf.sprintf "%s(%s)" v.variant_name (format_type typ)
      in
      Printf.sprintf "union %s { %s }" name
        (String.concat " | " (List.map format_variant variants))

(** Format a binary operator *)
let format_binop = function
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
  | Assign -> "="
  | Pipe -> "|>"

(** Format a unary operator *)
let format_unop = function Not -> "!" | Neg -> "-"

(** Format a pattern *)
let rec format_pattern = function
  | PWildcard -> "_"
  | PVar name -> name
  | PLiteral expr -> format_expr (create_context ()) expr
  | PTuple patterns ->
      "(" ^ String.concat ", " (List.map format_pattern patterns) ^ ")"
  | PArray patterns ->
      "[" ^ String.concat ", " (List.map format_pattern patterns) ^ "]"
  | PSome pattern -> Printf.sprintf "Some(%s)" (format_pattern pattern)
  | PNone -> "None"
  | POk pattern -> Printf.sprintf "Ok(%s)" (format_pattern pattern)
  | PErr pattern -> Printf.sprintf "Err(%s)" (format_pattern pattern)
  | PRange (min_expr, max_expr) ->
      Printf.sprintf "%s..%s"
        (format_expr (create_context ()) min_expr)
        (format_expr (create_context ()) max_expr)
  | PStruct field_patterns ->
      let format_field (name, pattern) =
        Printf.sprintf "%s: %s" name (format_pattern pattern)
      in
      "{ " ^ String.concat ", " (List.map format_field field_patterns) ^ " }"

(** Format an expression *)
and format_expr ctx = function
  (* Literals *)
  | EI8 n -> Printf.sprintf "%di8" n
  | EU8 n -> Printf.sprintf "%du8" n
  | EI16 n -> Printf.sprintf "%di16" n
  | EU16 n -> Printf.sprintf "%du16" n
  | EI32 n -> Printf.sprintf "%di32" n
  | EU32 n -> Printf.sprintf "%du32" n
  | EI64 n -> Printf.sprintf "%Ldi64" n
  | EU64 n -> Printf.sprintf "%Ldu64" n
  | EInt n -> Int64.to_string n
  | EUint n -> Printf.sprintf "%Ldu" n
  | EF32 f -> Printf.sprintf "%ff32" f
  | EF64 f -> Printf.sprintf "%ff64" f
  | EFloat f -> string_of_float f
  | EBool true -> "true"
  | EBool false -> "false"
  | EChar c -> Printf.sprintf "'%c'" c
  | EString s -> Printf.sprintf "\"%s\"" s
  | EBytes bytes ->
      Printf.sprintf "b\"%s\"" (String.escaped (Bytes.to_string bytes))
  | EUnit -> "()"
  (* Variables and operators *)
  | EVar name -> name
  | EBinOp (op, lhs, rhs) ->
      Printf.sprintf "%s %s %s"
        (format_expr_paren ctx lhs)
        (format_binop op)
        (format_expr_paren ctx rhs)
  | EUnOp (op, expr) ->
      Printf.sprintf "%s%s" (format_unop op) (format_expr_paren ctx expr)
  (* Control flow *)
  | EIf (cond, then_expr, else_expr) ->
      let cond_str = format_expr ctx cond in
      let then_str = format_expr (indent_more ctx) then_expr in
      let else_str = format_expr (indent_more ctx) else_expr in
      Printf.sprintf "if %s {\n%s%s\n%s} else {\n%s%s\n%s}" cond_str
        (indent (indent_more ctx))
        then_str (indent ctx)
        (indent (indent_more ctx))
        else_str (indent ctx)
  | EWhile (cond, body) ->
      Printf.sprintf "while %s {\n%s%s\n%s}" (format_expr ctx cond)
        (indent (indent_more ctx))
        (format_expr (indent_more ctx) body)
        (indent ctx)
  | EFor (var, iter, body) ->
      Printf.sprintf "for %s in %s {\n%s%s\n%s}" var (format_expr ctx iter)
        (indent (indent_more ctx))
        (format_expr (indent_more ctx) body)
        (indent ctx)
  | EMatch (scrutinee, arms) ->
      let format_arm arm =
        let pattern_str = format_pattern arm.pattern in
        let guard_str =
          match arm.guard with
          | None -> ""
          | Some g -> Printf.sprintf " if %s" (format_expr ctx g)
        in
        let body_str = format_expr (indent_more ctx) arm.body in
        Printf.sprintf "%s%s%s => %s"
          (indent (indent_more ctx))
          pattern_str guard_str body_str
      in
      Printf.sprintf "match %s {\n%s\n%s}"
        (format_expr ctx scrutinee)
        (String.concat ",\n" (List.map format_arm arms))
        (indent ctx)
  (* Functions *)
  | EFun (params, ret_type, body) ->
      let format_param p =
        Printf.sprintf "%s: %s" p.name (format_type p.param_type)
      in
      let params_str = String.concat ", " (List.map format_param params) in
      let ret_str = format_type ret_type in
      let body_str = format_expr (indent_more ctx) body in
      Printf.sprintf "fn(%s) -> %s {\n%s%s\n%s}" params_str ret_str
        (indent (indent_more ctx))
        body_str (indent ctx)
  | ECall (func, args) ->
      Printf.sprintf "%s(%s)" (format_expr ctx func)
        (String.concat ", " (List.map (format_expr ctx) args))
  (* Let binding *)
  | ELet (name, value, body) ->
      Printf.sprintf "let %s = %s in\n%s%s" name (format_expr ctx value)
        (indent ctx) (format_expr ctx body)
  (* Data structures *)
  | EBlock exprs ->
      if exprs = [] then "()"
      else
        let formatted = List.map (format_expr (indent_more ctx)) exprs in
        Printf.sprintf "{\n%s\n%s}"
          (String.concat
             (";\n" ^ indent (indent_more ctx))
             (List.map (fun s -> indent (indent_more ctx) ^ s) formatted))
          (indent ctx)
  | EArray exprs ->
      "[" ^ String.concat ", " (List.map (format_expr ctx) exprs) ^ "]"
  | EArrayAccess (arr, idx) ->
      Printf.sprintf "%s[%s]" (format_expr ctx arr) (format_expr ctx idx)
  | ETuple exprs ->
      "(" ^ String.concat ", " (List.map (format_expr ctx) exprs) ^ ")"
  | EStruct field_inits ->
      let format_init init =
        Printf.sprintf "%s: %s" init.init_field_name
          (format_expr ctx init.init_value)
      in
      "{ " ^ String.concat ", " (List.map format_init field_inits) ^ " }"
  | EFieldAccess (expr, field) ->
      Printf.sprintf "%s.%s" (format_expr ctx expr) field
  (* Option/Result constructors *)
  | ESome expr -> Printf.sprintf "Some(%s)" (format_expr ctx expr)
  | ENone -> "None"
  | EOk expr -> Printf.sprintf "Ok(%s)" (format_expr ctx expr)
  | EErr expr -> Printf.sprintf "Err(%s)" (format_expr ctx expr)
  (* Built-in functions *)
  | EPrint expr -> Printf.sprintf "print(%s)" (format_expr ctx expr)
  | EPrintln expr -> Printf.sprintf "println(%s)" (format_expr ctx expr)
  | EUnsafe expr -> Printf.sprintf "unsafe(%s)" (format_expr ctx expr)

(* Type annotations and other - not yet in AST *)
(* These would be added when the AST supports them
  | ETypeAnnotation (expr, typ) ->
      Printf.sprintf "(%s: %s)" (format_expr ctx expr) (format_type typ)
  | EReturn expr -> Printf.sprintf "return %s" (format_expr ctx expr)
  | EBreak -> "break"
  | EContinue -> "continue"
  *)

(** Format expression with parentheses if needed *)
and format_expr_paren ctx expr =
  match expr with
  | EBinOp _ | EUnOp _ -> "(" ^ format_expr ctx expr ^ ")"
  | _ -> format_expr ctx expr

(** Format a complete program including package, imports, types, and functions
*)
let format_program ctx program =
  let parts = [] in

  (* Format package declaration *)
  let parts = Printf.sprintf "package %s" program.package_name :: parts in

  (* Add empty line after package *)
  let parts = "" :: parts in

  (* Format imports (if any) *)
  let parts =
    if program.imports = [] then parts
    else
      let import_strs =
        List.map
          (fun import ->
            match import.alias with
            | None -> Printf.sprintf "import \"%s\"" import.package_path
            | Some alias ->
                Printf.sprintf "import %s \"%s\"" alias import.package_path)
          program.imports
      in
      let parts = ("" :: List.rev import_strs) @ parts in
      parts
  in

  (* Format type definitions (if any) *)
  let parts =
    if program.type_defs = [] then parts
    else
      (* TODO: Implement type definition formatting *)
      parts
  in

  (* Format functions *)
  let parts =
    if program.functions = [] then parts
    else
      let func_strs =
        List.map
          (fun func ->
            let params_str =
              if func.params = [] then "()"
              else
                let param_strs =
                  List.map
                    (fun (name, typ) ->
                      Printf.sprintf "%s: %s" name (format_type typ))
                    func.params
                in
                Printf.sprintf "(%s)" (String.concat ", " param_strs)
            in
            let ret_str = format_type func.ret in
            let body_str =
              match func.body with
              | [ SExpr (EBlock exprs) ] ->
                  (* Body is already a block, format the expressions inside *)
                  let formatted =
                    List.map (format_expr (indent_more ctx)) exprs
                  in
                  String.concat
                    (";\n" ^ indent (indent_more ctx))
                    (List.map (fun s -> indent (indent_more ctx) ^ s) formatted)
              | [ SExpr expr ] ->
                  (* Single expression, format without block *)
                  format_expr ctx expr
              | _ -> "/* complex body not supported */"
            in
            (* Use block syntax only if the body is already a block *)
            match func.body with
            | [ SExpr (EBlock _) ] ->
                Printf.sprintf "let %s%s: %s = {\n%s\n%s}" func.fname params_str
                  ret_str body_str (indent ctx)
            | [ SExpr _ ] ->
                Printf.sprintf "let %s%s: %s =\n%s%s" func.fname params_str
                  ret_str
                  (indent (indent_more ctx))
                  body_str
            | _ ->
                Printf.sprintf "let %s%s: %s = {\n%s\n%s}" func.fname params_str
                  ret_str body_str (indent ctx))
          program.functions
      in
      ("" :: List.rev func_strs) @ parts
  in

  String.concat "\n" (List.rev parts)

(** Format a program - now supports complete program structure *)

(** Main formatting function that takes source code and returns formatted code
*)
let format_source source =
  let tokens = Lexer.lex source in
  match Parser.parse_program_with_error_handling tokens with
  | Ok program -> format_program (create_context ()) program
  | Error msg -> raise (Failure ("Parse error: " ^ msg))

(** Format a file in place (like go fmt) *)
let format_file filename =
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;

  try
    let formatted = format_source source in
    let oc = open_out filename in
    output_string oc formatted;
    output_char oc '\n';
    close_out oc;
    Ok ()
  with
  | Lexer.Lex_error msg -> Error (Printf.sprintf "Lexer error: %s" msg)
  | Parser.Parse_error msg -> Error (Printf.sprintf "Parser error: %s" msg)
  | Failure msg -> Error msg
  | e -> Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(** Format multiple files *)
let format_files filenames =
  List.fold_left
    (fun acc filename ->
      match acc with
      | Error _ -> acc
      | Ok () -> (
          match format_file filename with
          | Ok () ->
              Printf.printf "Formatted: %s\n" filename;
              Ok ()
          | Error msg ->
              Error (Printf.sprintf "Error formatting %s: %s" filename msg)))
    (Ok ()) filenames
