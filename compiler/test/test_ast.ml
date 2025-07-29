open Alcotest
open Ast

(* Pretty-printers for testing *)
let rec pp_typ fmt = function
  | TVar id -> Format.fprintf fmt "TVar(%d)" id
  | TI8 -> Format.fprintf fmt "TI8"
  | TU8 -> Format.fprintf fmt "TU8"
  | TI16 -> Format.fprintf fmt "TI16"
  | TU16 -> Format.fprintf fmt "TU16"
  | TI32 -> Format.fprintf fmt "TI32"
  | TU32 -> Format.fprintf fmt "TU32"
  | TI64 -> Format.fprintf fmt "TI64"
  | TU64 -> Format.fprintf fmt "TU64"
  | TInt -> Format.fprintf fmt "TInt"
  | TUint -> Format.fprintf fmt "TUint"
  | TF32 -> Format.fprintf fmt "TF32"
  | TF64 -> Format.fprintf fmt "TF64"
  | TFloat -> Format.fprintf fmt "TFloat"
  | TBool -> Format.fprintf fmt "TBool"
  | TChar -> Format.fprintf fmt "TChar"
  | TString -> Format.fprintf fmt "TString"
  | TBytes -> Format.fprintf fmt "TBytes"
  | TUnit -> Format.fprintf fmt "TUnit"
  | TArray (t, None) -> Format.fprintf fmt "TArray(%a, None)" pp_typ t
  | TArray (t, Some n) -> Format.fprintf fmt "TArray(%a, Some %d)" pp_typ t n
  | TTuple ts ->
      Format.fprintf fmt "TTuple([%s])"
        (String.concat "; " (List.map (Format.asprintf "%a" pp_typ) ts))
  | TOption t -> Format.fprintf fmt "TOption(%a)" pp_typ t
  | TResult (t, e) -> Format.fprintf fmt "TResult(%a, %a)" pp_typ t pp_typ e
  | TNever -> Format.fprintf fmt "TNever"
  | TFunc (args, ret) ->
      Format.fprintf fmt "TFunc([%s], %a)"
        (String.concat "; " (List.map (Format.asprintf "%a" pp_typ) args))
        pp_typ ret
  | TStruct fields ->
      let field_str =
        String.concat "; "
          (List.map
             (fun f ->
               Format.asprintf "%s: %a" f.field_name pp_typ f.field_type)
             fields)
      in
      Format.fprintf fmt "TStruct([%s])" field_str
  | TUnion (name, variants) ->
      let variant_str =
        String.concat "; "
          (List.map
             (fun v ->
               match v.variant_data with
               | None -> v.variant_name
               | Some t -> Format.asprintf "%s(%a)" v.variant_name pp_typ t)
             variants)
      in
      Format.fprintf fmt "TUnion(%s, [%s])" name variant_str

let eq_typ = ( = )
let testable_typ = Alcotest.testable pp_typ eq_typ

(* Printer for binary operators *)
let pp_binop fmt = function
  | Add -> Format.fprintf fmt "Add"
  | Sub -> Format.fprintf fmt "Sub"
  | Mul -> Format.fprintf fmt "Mul"
  | Div -> Format.fprintf fmt "Div"
  | Mod -> Format.fprintf fmt "Mod"
  | Eq -> Format.fprintf fmt "Eq"
  | Neq -> Format.fprintf fmt "Neq"
  | Lt -> Format.fprintf fmt "Lt"
  | Le -> Format.fprintf fmt "Le"
  | Gt -> Format.fprintf fmt "Gt"
  | Ge -> Format.fprintf fmt "Ge"
  | And -> Format.fprintf fmt "And"
  | Or -> Format.fprintf fmt "Or"
  | Assign -> Format.fprintf fmt "Assign"
  | Pipe -> Format.fprintf fmt "Pipe"

let rec pp_expr fmt = function
  | EI8 n -> Format.fprintf fmt "EI8(%d)" n
  | EU8 n -> Format.fprintf fmt "EU8(%d)" n
  | EI16 n -> Format.fprintf fmt "EI16(%d)" n
  | EU16 n -> Format.fprintf fmt "EU16(%d)" n
  | EI32 n -> Format.fprintf fmt "EI32(%d)" n
  | EU32 n -> Format.fprintf fmt "EU32(%d)" n
  | EI64 n -> Format.fprintf fmt "EI64(%Ld)" n
  | EU64 n -> Format.fprintf fmt "EU64(%Ld)" n
  | EInt n -> Format.fprintf fmt "EInt(%Ld)" n
  | EUint n -> Format.fprintf fmt "EUint(%Ld)" n
  | EF32 f -> Format.fprintf fmt "EF32(%g)" f
  | EF64 f -> Format.fprintf fmt "EF64(%g)" f
  | EFloat f -> Format.fprintf fmt "EFloat(%g)" f
  | EBool b -> Format.fprintf fmt "EBool(%b)" b
  | EChar c -> Format.fprintf fmt "EChar('%c')" c
  | EString s -> Format.fprintf fmt "EString(\"%s\")" s
  | EBytes _ -> Format.fprintf fmt "EBytes(<bytes>)"
  | EUnit -> Format.fprintf fmt "EUnit"
  | EVar s -> Format.fprintf fmt "EVar(%s)" s
  | ETuple es ->
      Format.fprintf fmt "ETuple([%s])"
        (String.concat "; " (List.map (Format.asprintf "%a" pp_expr) es))
  | ESome e -> Format.fprintf fmt "ESome(%a)" pp_expr e
  | ENone -> Format.fprintf fmt "ENone"
  | EOk e -> Format.fprintf fmt "EOk(%a)" pp_expr e
  | EErr e -> Format.fprintf fmt "EErr(%a)" pp_expr e
  | EBinOp (op, l, r) ->
      Format.fprintf fmt "EBinOp(%a, %a, %a)" pp_binop op pp_expr l pp_expr r
  | _ -> Format.fprintf fmt "<complex expr>"

let eq_expr = ( = )
let testable_expr = Alcotest.testable pp_expr eq_expr

let test_primitive_types () =
  let cases =
    [
      ("i8 type", TI8, TI8);
      ("u8 type", TU8, TU8);
      ("i16 type", TI16, TI16);
      ("u16 type", TU16, TU16);
      ("i32 type", TI32, TI32);
      ("u32 type", TU32, TU32);
      ("i64 type", TI64, TI64);
      ("u64 type", TU64, TU64);
      ("int type", TInt, TInt);
      ("uint type", TUint, TUint);
      ("f32 type", TF32, TF32);
      ("f64 type", TF64, TF64);
      ("float type", TFloat, TFloat);
      ("bool type", TBool, TBool);
      ("char type", TChar, TChar);
      ("string type", TString, TString);
      ("bytes type", TBytes, TBytes);
      ("unit type", TUnit, TUnit);
      ("never type", TNever, TNever);
    ]
  in
  List.iter (fun (name, a, b) -> Alcotest.check testable_typ name a b) cases

let test_compound_types () =
  let cases =
    [
      ("array fixed", TArray (TInt, Some 3), TArray (TInt, Some 3));
      ("array var", TArray (TBool, None), TArray (TBool, None));
      ("tuple two", TTuple [ TInt; TString ], TTuple [ TInt; TString ]);
      ( "tuple three",
        TTuple [ TBool; TChar; TF32 ],
        TTuple [ TBool; TChar; TF32 ] );
      ("option int", TOption TInt, TOption TInt);
      ("option string", TOption TString, TOption TString);
      ("result ok", TResult (TInt, TString), TResult (TInt, TString));
      ( "func simple",
        TFunc ([ TInt; TBool ], TString),
        TFunc ([ TInt; TBool ], TString) );
      ("func no args", TFunc ([], TUnit), TFunc ([], TUnit));
    ]
  in
  List.iter (fun (name, a, b) -> Alcotest.check testable_typ name a b) cases

let test_primitive_exprs () =
  let cases =
    [
      ("i8 literal", EI8 42, EI8 42);
      ("u8 literal", EU8 255, EU8 255);
      ("i16 literal", EI16 (-1000), EI16 (-1000));
      ("u16 literal", EU16 65535, EU16 65535);
      ("i32 literal", EI32 2147483647, EI32 2147483647);
      ("u32 literal", EU32 (-1), EU32 (-1));
      ("i64 literal", EI64 9223372036854775807L, EI64 9223372036854775807L);
      ("u64 literal", EU64 9223372036854775807L, EU64 9223372036854775807L);
      ("int literal", EInt 42L, EInt 42L);
      ("uint literal", EUint 42L, EUint 42L);
      ("f32 literal", EF32 3.14, EF32 3.14);
      ("f64 literal", EF64 3.141592653589793, EF64 3.141592653589793);
      ("float literal", EFloat 2.71828, EFloat 2.71828);
      ("bool true", EBool true, EBool true);
      ("bool false", EBool false, EBool false);
      ("char literal", EChar 'A', EChar 'A');
      ("string literal", EString "hello", EString "hello");
      ("unit literal", EUnit, EUnit);
    ]
  in
  List.iter (fun (name, a, b) -> Alcotest.check testable_expr name a b) cases

let test_compound_exprs () =
  let cases =
    [
      ("var ref", EVar "x", EVar "x");
      ( "tuple two",
        ETuple [ EInt 1L; EString "a" ],
        ETuple [ EInt 1L; EString "a" ] );
      ( "tuple three",
        ETuple [ EBool true; EChar 'x'; EF32 1.0 ],
        ETuple [ EBool true; EChar 'x'; EF32 1.0 ] );
      ("some value", ESome (EInt 42L), ESome (EInt 42L));
      ("none value", ENone, ENone);
      ("ok value", EOk (EString "success"), EOk (EString "success"));
      ("err value", EErr (EString "error"), EErr (EString "error"));
      ("bin op", EBinOp (Add, EInt 1L, EInt 2L), EBinOp (Add, EInt 1L, EInt 2L));
    ]
  in
  List.iter (fun (name, a, b) -> Alcotest.check testable_expr name a b) cases

let test_type_utils () =
  let open TypeUtils in
  (* Test integer type checks *)
  Alcotest.(check bool) "i8 is integer" true (is_integer_type TI8);
  Alcotest.(check bool) "u64 is integer" true (is_integer_type TU64);
  Alcotest.(check bool) "int is integer" true (is_integer_type TInt);
  Alcotest.(check bool) "float is not integer" false (is_integer_type TFloat);
  Alcotest.(check bool) "bool is not integer" false (is_integer_type TBool);

  (* Test float type checks *)
  Alcotest.(check bool) "f32 is float" true (is_float_type TF32);
  Alcotest.(check bool) "f64 is float" true (is_float_type TF64);
  Alcotest.(check bool) "float is float" true (is_float_type TFloat);
  Alcotest.(check bool) "int is not float" false (is_float_type TInt);

  (* Test numeric type checks *)
  Alcotest.(check bool) "i32 is numeric" true (is_numeric_type TI32);
  Alcotest.(check bool) "f64 is numeric" true (is_numeric_type TF64);
  Alcotest.(check bool) "bool is not numeric" false (is_numeric_type TBool);

  (* Test signed/unsigned *)
  Alcotest.(check bool) "i8 is signed" true (is_signed_integer TI8);
  Alcotest.(check bool) "u8 is not signed" false (is_signed_integer TU8);
  Alcotest.(check bool) "u16 is unsigned" true (is_unsigned_integer TU16);
  Alcotest.(check bool) "i16 is not unsigned" false (is_unsigned_integer TI16);

  (* Test bit widths *)
  Alcotest.(check int) "i8 bit width" 8 (get_integer_bit_width TI8);
  Alcotest.(check int) "u16 bit width" 16 (get_integer_bit_width TU16);
  Alcotest.(check int) "i32 bit width" 32 (get_integer_bit_width TI32);
  Alcotest.(check int) "i64 bit width" 64 (get_integer_bit_width TI64);
  Alcotest.(check int) "int bit width" 64 (get_integer_bit_width TInt);

  Alcotest.(check int) "f32 bit width" 32 (get_float_bit_width TF32);
  Alcotest.(check int) "f64 bit width" 64 (get_float_bit_width TF64);
  Alcotest.(check int) "float bit width" 64 (get_float_bit_width TFloat);

  (* Test type equality *)
  Alcotest.(check bool) "int equals int" true (typ_equal TInt TInt);
  Alcotest.(check bool) "int not equals i64" false (typ_equal TInt TI64);
  Alcotest.(check bool)
    "tuple equals" true
    (typ_equal (TTuple [ TInt; TBool ]) (TTuple [ TInt; TBool ]));
  Alcotest.(check bool)
    "option equals" true
    (typ_equal (TOption TString) (TOption TString));
  Alcotest.(check bool)
    "result equals" true
    (typ_equal (TResult (TInt, TString)) (TResult (TInt, TString)));

  (* Test type to string *)
  Alcotest.(check string) "i8 to string" "i8" (typ_to_string TI8);
  Alcotest.(check string) "bool to string" "bool" (typ_to_string TBool);
  Alcotest.(check string)
    "tuple to string" "(int, bool)"
    (typ_to_string (TTuple [ TInt; TBool ]));
  Alcotest.(check string)
    "option to string" "Option<string>"
    (typ_to_string (TOption TString));
  Alcotest.(check string)
    "result to string" "Result<int, string>"
    (typ_to_string (TResult (TInt, TString)));
  Alcotest.(check string)
    "func to string" "(int, bool) -> string"
    (typ_to_string (TFunc ([ TInt; TBool ], TString)))

let () =
  run "SNOW Type System Tests"
    [
      ( "primitive types",
        [ test_case "primitive types" `Quick test_primitive_types ] );
      ( "compound types",
        [ test_case "compound types" `Quick test_compound_types ] );
      ( "primitive expressions",
        [ test_case "primitive expressions" `Quick test_primitive_exprs ] );
      ( "compound expressions",
        [ test_case "compound expressions" `Quick test_compound_exprs ] );
      ("type utilities", [ test_case "type utilities" `Quick test_type_utils ]);
    ]
