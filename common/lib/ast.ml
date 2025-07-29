(*** @project snow-toolchain @module AST

  Abstract Syntax Tree definitions for the SNOW programming language.

  This module serves as the central data structure representation for SNOW
  programs. It defines all AST node types that represent the structure of SNOW
  source code after parsing but before code generation.

  Design Philosophy: - Expression-based: SNOW is expression-oriented, so most
  constructs return values - Type safety: All nodes carry type information for
  static analysis - Immutability: AST nodes are immutable by design for safe
  compiler transformations - Simplicity: Direct mapping from language constructs
  to AST nodes

  Used by: - Parser: Constructs AST from token streams - Type checker: Validates
  types and annotates nodes - LLVM code generator: Traverses AST to emit LLVM IR
  - Tooling: IDE support, formatters, linters *)

(** Type system representation for SNOW language.

    SNOW has a rich type system with explicit sizes for numeric types, algebraic
    data types, and function types. This type covers all possible types that can
    appear in SNOW programs.

    Design notes:
    - Explicit bit widths for integers prevent implicit conversions
    - TInt/TUint are aliases but distinct types for compatibility
    - TArray distinguishes fixed-size arrays from dynamic slices
    - TNever represents computations that never return (infinite loops, panics)
*)
type typ =
  (* === Type variables for Hindley-Milner inference === *)
  | TVar of int
      (** Type variable: Used during type inference, identified by unique
          integer *)
  (* === Primitive numeric types === *)
  | TI8  (** 8-bit signed integer: range -128 to 127 *)
  | TU8  (** 8-bit unsigned integer: range 0 to 255 *)
  | TI16  (** 16-bit signed integer: range -32,768 to 32,767 *)
  | TU16  (** 16-bit unsigned integer: range 0 to 65,535 *)
  | TI32  (** 32-bit signed integer: range ±2.1 billion *)
  | TU32  (** 32-bit unsigned integer: range 0 to 4.3 billion *)
  | TI64  (** 64-bit signed integer: range ±9.2 × 10^18 *)
  | TU64  (** 64-bit unsigned integer: range 0 to 1.8 × 10^19 *)
  | TInt  (** Default signed integer (currently i64, may change) *)
  | TUint  (** Default unsigned integer (currently u64, may change) *)
  (* === Floating point types === *)
  | TF32  (** 32-bit IEEE 754 floating point: ~7 decimal digits precision *)
  | TF64  (** 64-bit IEEE 754 floating point: ~15-17 decimal digits precision *)
  | TFloat  (** Default float type (currently f64 alias) *)
  (* === Other primitive types === *)
  | TBool  (** Boolean type: true or false *)
  | TChar  (** Unicode scalar value (UTF-32): represents a single character *)
  | TString  (** UTF-8 encoded immutable string *)
  | TBytes  (** Binary blob: array of raw bytes for low-level operations *)
  | TUnit  (** Unit type: represents void/empty value, written as () *)
  (* === Composite types === *)
  | TArray of typ * int option
      (** Array types:
          - TArray(T, Some n): Fixed-length array [T; n] allocated on stack
          - TArray(T, None): Variable-length slice [T] allocated on heap *)
  | TTuple of typ list
      (** Tuple type with heterogeneous elements: (T1, T2, ..., Tn) Used for
          multiple return values and structured data *)
  | TOption of typ
      (** Option<T> type: represents nullable values safely
          - Some(value): contains a value of type T
          - None: represents absence of value *)
  | TResult of typ * typ
      (** Result<T, E> type: represents computations that may fail
          - Ok(value): successful result with value of type T
          - Err(error): failure with error of type E *)
  | TNever
      (** Never type (!): represents computations that never return normally
          Used for infinite loops, explicit panics, diverging functions *)
  | TFunc of typ list * typ
      (** Function type: (T1, T2, ..., Tn) -> R First-class functions with
          argument types and return type *)
  | TStruct of struct_field list
      (** Struct type with named fields. Each field has a name and type. *)
  | TUnion of string * union_variant list
      (** Union/ADT type with variants. First string is the type name. *)

and struct_field = {
  field_name : string;  (** Field name (case determines visibility) *)
  field_type : typ;  (** Type of the field *)
}

and union_variant = {
  variant_name : string;  (** Variant name (case determines visibility) *)
  variant_data : typ option;  (** Optional associated data type *)
}

(** Unary operators in SNOW.

    Currently only logical negation is supported. Future extensions may include
    arithmetic negation (-x), bitwise not (~x). *)
type unop =
  | Not  (** Logical negation (!): converts true to false and vice versa *)
  | Neg  (** Arithmetic negation (-): numeric negation for numbers *)

(** Binary operators in SNOW with their semantic meanings.

    These operators follow common precedence rules:
    - Arithmetic: *, /, % (highest) then +, -
    - Comparison: ==, !=, <, <=, >, >=
    - Logical: && (higher) then || (lowest)

    Type checking ensures operands are compatible. *)
type binop =
  (* === Arithmetic operators === *)
  | Add  (** Addition (+): numeric types and string concatenation *)
  | Sub  (** Subtraction (-): numeric types only *)
  | Mul  (** Multiplication ( * ): numeric types only *)
  | Div  (** Division (/): numeric types, may raise division by zero *)
  | Mod  (** Modulo (%): integer types only, may raise division by zero *)
  (* === Equality operators === *)
  | Eq  (** Equality (==): structural equality for all types *)
  | Neq  (** Inequality (!=): negation of structural equality *)
  (* === Ordering operators === *)
  | Lt  (** Less than (<): ordered types (numbers, chars, strings) *)
  | Le  (** Less or equal (<=): ordered types *)
  | Gt  (** Greater than (>): ordered types *)
  | Ge  (** Greater or equal (>=): ordered types *)
  (* === Logical operators === *)
  | And  (** Logical AND (&&): boolean types, short-circuiting *)
  | Or  (** Logical OR (||): boolean types, short-circuiting *)
  (* === Assignment and pipe operators === *)
  | Assign
      (** Assignment (:=): mutable variable assignment, right-associative *)
  | Pipe  (** Pipe (|>): function composition, left-associative *)

(** Expression nodes representing all SNOW computations.

    SNOW is expression-based, meaning all constructs produce values. This type
    represents the full range of expressions available:

    - Literals: Direct values embedded in source code
    - Variables: References to bound identifiers
    - Operations: Computations combining sub-expressions
    - Constructors: Building composite values (tuples, Option, Result)
    - Control flow: Conditional expressions and pattern matching

    Design principles:
    - Each expression has a deterministic type
    - Side effects are explicit (mutable operations, I/O)
    - Nested structure allows complex expressions *)
type expr =
  (* === Literal expressions === *)
  | EI8 of int  (** 8-bit signed integer literal: 42i8 *)
  | EU8 of int  (** 8-bit unsigned integer literal: 255u8 *)
  | EI16 of int  (** 16-bit signed integer literal: 1000i16 *)
  | EU16 of int  (** 16-bit unsigned integer literal: 65535u16 *)
  | EI32 of int  (** 32-bit signed integer literal: 42i32 *)
  | EU32 of int  (** 32-bit unsigned integer literal: 42u32 *)
  | EI64 of int64  (** 64-bit signed integer literal: 42i64 *)
  | EU64 of int64  (** 64-bit unsigned integer literal: 42u64 *)
  | EInt of int64  (** Default integer literal: 42 (inferred as int/i64) *)
  | EUint of int64  (** Explicit unsigned integer literal: 42uint *)
  | EF32 of float  (** 32-bit float literal: 3.14f32 *)
  | EF64 of float  (** 64-bit float literal: 3.14f64 *)
  | EFloat of float  (** Default float literal: 3.14 (inferred as float/f64) *)
  | EBool of bool  (** Boolean literal: true or false *)
  | EChar of char  (** Character literal: 'A', '\n', '\u{1F4A9}' *)
  | EString of string  (** String literal: "hello", "world\n" *)
  | EBytes of bytes  (** Bytes literal: [0x48, 0x65, 0x6C, 0x6C, 0x6F] *)
  | EUnit  (** Unit literal: () *)
  (* === Variable and access expressions === *)
  | EVar of string  (** Variable reference: variable name lookup *)
  | EArrayAccess of expr * expr
      (** Array indexing: arr[index] First expr is array/slice, second is index
          expression *)
  | ECall of expr * expr list
      (** Function call: function_expr(arg1, arg2, ...) First expr is the
          function to call (can be variable, function expression, or any
          expression that evaluates to a function), list contains argument
          expressions *)
  (* === Operator expressions === *)
  | EBinOp of binop * expr * expr
      (** Binary operation: left_expr OP right_expr Operator precedence handled
          during parsing *)
  | EUnOp of unop * expr
      (** Unary operation: OP expr Currently only logical negation (!expr) *)
  (* === Constructor expressions === *)
  | ETuple of expr list
      (** Tuple construction: (expr1, expr2, ...) Creates tuple value with
          heterogeneous elements *)
  | EArray of expr list
      (** Array literal: [expr1, expr2, ...] Creates array value with
          homogeneous elements. Empty array [] is valid. *)
  | ESome of expr
      (** Option Some constructor: Some(value) Wraps value in Some variant of
          Option type *)
  | ENone
      (** Option None constructor: None Represents absence of value in Option
          type *)
  | EOk of expr
      (** Result Ok constructor: Ok(value) Represents successful result in
          Result type *)
  | EErr of expr
      (** Result Err constructor: Err(error) Represents failed result in Result
          type *)
  (* === Control flow expressions === *)
  | EIf of expr * expr * expr
      (** Conditional expression: if condition then true_branch else
          false_branch All three parts are expressions, making this a true
          expression-oriented construct. The condition must evaluate to a
          boolean type. *)
  | EMatch of expr * match_arm list
      (** Match expression: match scrutinee with | pattern -> expr Pattern
          matching allows destructuring of algebraic data types and provides
          exhaustive case analysis. The scrutinee is evaluated once and matched
          against each pattern in order until one matches. *)
  | ELet of string * expr * expr
      (** Let expression: let identifier = value_expr in body_expr Local
          variable binding that introduces a new identifier in the scope of
          body_expr. The value_expr is evaluated first, then its result is bound
          to identifier for use within body_expr. *)
  | EFun of param list * typ * expr
      (** Function expression: fun(param1, param2, ...) -> return_type = body
          Function definition with parameter list, return type annotation, and
          function body expression. Creates a first-class function value that
          can be bound to variables or passed as arguments. *)
  | EBlock of expr list
      (** Block expression: { expr1; expr2; ...; exprN } 
          A sequence of expressions evaluated in order. All expressions except
          the last are evaluated for their side effects, and the last expression's
          value becomes the block's result. Empty blocks return unit. *)
  | EWhile of expr * expr
      (** While loop expression: while condition do body Evaluates body
          repeatedly while condition is true. Returns unit. *)
  | EFor of string * expr * expr
      (** For loop expression: for var in iterable do body Iterates over
          iterable, binding each element to var. Returns unit. *)
  | EStruct of struct_field_init list
      (** Struct literal: { field_name = expr, ... }
          Creates a struct value by explicitly naming fields and their values.
          Used with user-defined struct types. Field order is flexible. *)
  | EFieldAccess of expr * string
      (** Field access: expr.field_name Accesses a named field of a struct
          value. First expr must evaluate to a struct type that contains the
          named field. *)
  | EUnsafe of expr
      (** Unsafe expression: unsafe(expr) Marks code that may be unsafe *)
  (* === Built-in functions === *)
  | EPrint of expr
      (** Built-in print function: print(expr) Outputs the value to stdout
          without a newline. Works with all printable types. *)
  | EPrintln of expr
      (** Built-in println function: println(expr) Outputs the value to stdout
          with a newline. Works with all printable types. *)

and param = {
  name : string;  (** Parameter name used for binding in function body *)
  param_type : typ;  (** Type annotation for the parameter *)
}
(** Function parameter definition for SNOW function expressions.

    Each parameter consists of a name and type annotation. Used in function
    definitions to specify the function's signature and enable type checking. *)

and struct_field_init = {
  init_field_name : string;  (** Field name being initialized *)
  init_value : expr;  (** Expression providing the field value *)
}

(** Pattern matching constructs for SNOW match expressions.

    Patterns are used in match expressions to destructure values and bind
    variables. They support both literal matching and structural decomposition
    of algebraic data types like Option and Result.

    Design principles:
    - Exhaustiveness checking: All possible cases must be covered
    - Variable binding: Patterns can bind values to identifiers
    - Nested patterns: Complex destructuring through pattern composition *)
and pattern =
  | PWildcard  (** Wildcard pattern (_): matches any value without binding *)
  | PVar of string  (** Variable pattern: binds matched value to identifier *)
  | PLiteral of expr
      (** Literal pattern: matches exact literal values (numbers, strings,
          booleans) *)
  | PSome of pattern
      (** Some pattern: Some(pattern) for Option type destructuring *)
  | PNone  (** None pattern: None for Option type *)
  | POk of pattern  (** Ok pattern: Ok(pattern) for Result type success case *)
  | PErr of pattern  (** Err pattern: Err(pattern) for Result type error case *)
  | PTuple of pattern list
      (** Tuple pattern: (p1, p2, ...) for tuple destructuring *)
  | PArray of pattern list
      (** Array pattern: [p1, p2, ...] for array destructuring *)
  | PRange of expr * expr
      (** Range pattern: min..max for numeric range matching *)
  | PStruct of (string * pattern) list
      (** Struct pattern: {field1: p1, field2: p2, ...} for struct destructuring *)

and match_arm = {
  pattern : pattern;  (** Pattern to match against the scrutinee *)
  guard : expr option;  (** Optional guard condition (when clause) *)
  body : expr;  (** Expression to evaluate when pattern matches *)
}
(** Match arm representing a single case in a match expression.

    Contains a pattern to match against and an expression to evaluate when the
    pattern matches. Match arms are evaluated in order until the first matching
    pattern is found. *)

(** Statements in SNOW (currently limited, as SNOW is expression-based).

    Note: SNOW is primarily expression-based, so statements are mainly used for
    side effects and control flow. Most language constructs are expressions that
    produce values. Statements are being phased out in favor of expression
    equivalents (e.g., if expressions instead of if statements). *)
type stmt =
  | SVarDecl of string * typ * expr option
      (** Variable declaration: let name: type = expr Optional expr allows for
          uninitialized variables (filled with default) *)
  | SAssign of expr * expr
      (** Assignment to mutable variables: lhs := rhs Left side must be a
          mutable variable or array element *)
  | SExpr of expr
      (** Expression statement: evaluates expression and discards result Used
          for side effects like function calls *)
  | SIf of expr * stmt list * stmt list option
      (** If statement: if condition then statements [else statements] Being
          replaced by if expressions in expression contexts *)
  | SWhile of expr * stmt list
      (** While loop: while condition do statements Executes statements
          repeatedly while condition is true *)
  | SReturn of expr
      (** Return statement: return expr Early return from function with value *)

type func = {
  fname : string;  (** Function name (follows naming conventions) *)
  params : (string * typ) list;  (** Parameter list: (name, type) pairs *)
  ret : typ;  (** Return type annotation *)
  body : stmt list;  (** Function body as statement sequence *)
  is_extern : bool;  (** Whether this is an external function declaration *)
}
(** Function declaration structure.

    Functions are first-class values in SNOW and can be passed around, stored in
    variables, and used in higher-order programming.

    Design notes:
    - fname follows SNOW naming conventions (CamelCase for public, camelCase for
      private)
    - params support labeled arguments for clarity
    - body contains statements but functions should prefer expression-based
      implementation *)

type import_decl = {
  package_path : string;
      (** Import path like "math" or "github.com/user/pkg" *)
  alias : string option;  (** Optional alias for the imported package *)
}
(** Package import declaration for SNOW packages.

    Represents import statements that bring external packages into scope.
    Follows Go-like package system where imports are explicit and path-based. *)

(** Type definitions for user-defined types in SNOW.

    SNOW supports nominal typing with explicit type definitions:
    - Type aliases: type UserName = string
    - Struct types: type Point = struct { x: int; y: int }
    - Union types: type Option[T] = union { Some(T) | None }

    Design principles:
    - Nominal typing: UserName and string are distinct types
    - Visibility: Capitalized names are public, lowercase are private
    - Generic support: Built-in parameterized types like Option[T] *)
type type_def =
  | TAlias of string * typ  (** Type alias: type UserName = string *)
  | TStruct of string * struct_field list  (** Struct definition with fields *)
  | TUnion of string * union_variant list  (** Union/ADT definition *)

type program = {
  package_name : string;  (** Package declaration: package math *)
  imports : import_decl list;  (** Import declarations *)
  type_defs : type_def list;  (** User-defined types *)
  functions : func list;  (** Function definitions *)
}
(** Complete SNOW program representation with package system support.

    A SNOW program consists of:
    - Package declaration (required at top of each file)
    - Import declarations (explicit dependencies)
    - Type definitions (user-defined types)
    - Function definitions (the main program logic)
    - Global constants and variables (future extension)

    Design follows Go's package system:
    - One package per directory
    - Explicit imports with path-based names
    - Visibility controlled by identifier case *)

(** Type utility functions for SNOW type system analysis.

    This module provides helper functions for working with SNOW types. Used
    extensively by:
    - Type checker: Validating type compatibility and inference
    - Code generator: Selecting appropriate LLVM types and operations
    - Compiler optimizations: Type-specific optimizations
    - Tooling: IDE support, refactoring, analysis *)
module TypeUtils = struct
  (** Check if a type is a primitive integer type.

      Returns true for all integer variants including signed/unsigned and
      different bit widths. Used for:
      - Arithmetic operation validation
      - Literal type checking
      - Overflow analysis *)
  let is_integer_type = function
    | TI8 | TU8 | TI16 | TU16 | TI32 | TU32 | TI64 | TU64 | TInt | TUint -> true
    | _ -> false

  (** Check if a type is a primitive floating-point type.

      Returns true for f32, f64, and the float alias. Used for:
      - Arithmetic operation validation
      - Precision analysis
      - IEEE 754 compliance checks *)
  let is_float_type = function TF32 | TF64 | TFloat -> true | _ -> false

  (** Check if a type is numeric (integer or floating-point).

      Convenience function combining integer and float checks. Used for
      validating arithmetic expressions and numeric literals. *)
  let is_numeric_type t = is_integer_type t || is_float_type t

  (** Check if an integer type is signed.

      Used for:
      - Range checking (signed vs unsigned ranges)
      - Arithmetic operation semantics
      - Code generation (signed vs unsigned LLVM instructions) *)
  let is_signed_integer = function
    | TI8 | TI16 | TI32 | TI64 | TInt -> true
    | _ -> false

  (** Check if an integer type is unsigned.

      Complement of is_signed_integer. Used for similar purposes but for
      unsigned-specific logic and optimizations. *)
  let is_unsigned_integer = function
    | TU8 | TU16 | TU32 | TU64 | TUint -> true
    | _ -> false

  (** Get the bit width of integer types.

      Returns the number of bits used to represent the integer. Used for:
      - Overflow checking during constant folding
      - LLVM type generation (i8, i16, i32, i64)
      - Memory layout calculations
      - Range validation *)
  let get_integer_bit_width = function
    | TI8 | TU8 -> 8
    | TI16 | TU16 -> 16
    | TI32 | TU32 -> 32
    | TI64 | TU64 | TInt | TUint -> 64
    | _ -> failwith "get_integer_bit_width: Not an integer type"

  (** Get the bit width of floating-point types.

      Returns the number of bits in the IEEE 754 representation. Used for:
      - LLVM type generation (float, double)
      - Precision analysis and warnings
      - Memory layout calculations *)
  let get_float_bit_width = function
    | TF32 -> 32
    | TF64 | TFloat -> 64
    | _ -> failwith "get_float_bit_width: Not a float type"

  (** Convert type to human-readable string representation.

      Produces the same syntax used in SNOW source code for type annotations.
      Used for:
      - Error messages and diagnostics
      - IDE hover information
      - Debug output and logging
      - Pretty-printing AST nodes *)
  let rec typ_to_string = function
    | TVar id -> Printf.sprintf "'%d" id
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
    | TArray (t, Some n) -> Printf.sprintf "[%s; %d]" (typ_to_string t) n
    | TArray (t, None) -> Printf.sprintf "[%s]" (typ_to_string t)
    | TTuple ts ->
        Printf.sprintf "(%s)" (String.concat ", " (List.map typ_to_string ts))
    | TOption t -> Printf.sprintf "Option<%s>" (typ_to_string t)
    | TResult (t, e) ->
        Printf.sprintf "Result<%s, %s>" (typ_to_string t) (typ_to_string e)
    | TNever -> "!"
    | TFunc (args, ret) ->
        let arg_str = String.concat ", " (List.map typ_to_string args) in
        Printf.sprintf "(%s) -> %s" arg_str (typ_to_string ret)
    | TStruct fields ->
        let field_str =
          String.concat ", "
            (List.map
               (fun f ->
                 Printf.sprintf "%s: %s" f.field_name
                   (typ_to_string f.field_type))
               fields)
        in
        Printf.sprintf "{ %s }" field_str
    | TUnion (name, variants) ->
        let variant_str =
          String.concat " | "
            (List.map
               (fun v ->
                 match v.variant_data with
                 | None -> v.variant_name
                 | Some t ->
                     Printf.sprintf "%s(%s)" v.variant_name (typ_to_string t))
               variants)
        in
        Printf.sprintf "%s = %s" name variant_str

  (** Check structural type equality.

      Performs deep structural comparison of two types. Two types are equal if
      they have the same structure and all nested components are equal.

      Used for:
      - Type checking during compilation
      - Function signature matching
      - Generic type instantiation
      - Compiler optimizations and caching

      Note: This is structural equality, not nominal equality. In future
      versions with user-defined types, nominal equality may be needed for
      distinct types with same structure. *)
  let rec typ_equal t1 t2 =
    match (t1, t2) with
    | TI8, TI8
    | TU8, TU8
    | TI16, TI16
    | TU16, TU16
    | TI32, TI32
    | TU32, TU32
    | TI64, TI64
    | TU64, TU64
    | TInt, TInt
    | TUint, TUint
    | TF32, TF32
    | TF64, TF64
    | TFloat, TFloat
    | TBool, TBool
    | TChar, TChar
    | TString, TString
    | TBytes, TBytes
    | TUnit, TUnit
    | TNever, TNever ->
        true
    | TArray (t1, n1), TArray (t2, n2) -> typ_equal t1 t2 && n1 = n2
    | TTuple ts1, TTuple ts2 ->
        List.length ts1 = List.length ts2 && List.for_all2 typ_equal ts1 ts2
    | TOption t1, TOption t2 -> typ_equal t1 t2
    | TResult (t1, e1), TResult (t2, e2) -> typ_equal t1 t2 && typ_equal e1 e2
    | TFunc (args1, ret1), TFunc (args2, ret2) ->
        List.length args1 = List.length args2
        && List.for_all2 typ_equal args1 args2
        && typ_equal ret1 ret2
    | TStruct fields1, TStruct fields2 ->
        List.length fields1 = List.length fields2
        && List.for_all2
             (fun f1 f2 ->
               f1.field_name = f2.field_name
               && typ_equal f1.field_type f2.field_type)
             fields1 fields2
    | TUnion (name1, variants1), TUnion (name2, variants2) ->
        name1 = name2
        && List.length variants1 = List.length variants2
        && List.for_all2
             (fun v1 v2 ->
               v1.variant_name = v2.variant_name
               &&
               match (v1.variant_data, v2.variant_data) with
               | None, None -> true
               | Some t1, Some t2 -> typ_equal t1 t2
               | _ -> false)
             variants1 variants2
    | _ -> false
end
