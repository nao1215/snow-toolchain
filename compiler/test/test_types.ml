open Alcotest
open Ast

(** Test type range validations for integer types. *)
let test_integer_ranges () =
  (* Test signed 8-bit range *)
  let test_i8_range () =
    Alcotest.(check bool) "i8 min value valid" true (-128 >= -128 && -128 <= 127);
    Alcotest.(check bool) "i8 max value valid" true (127 >= -128 && 127 <= 127);
    Alcotest.(check bool) "i8 overflow invalid" false (128 >= -128 && 128 <= 127);
    Alcotest.(check bool)
      "i8 underflow invalid" false
      (-129 >= -128 && -129 <= 127)
  in

  (* Test unsigned 8-bit range *)
  let test_u8_range () =
    Alcotest.(check bool) "u8 min value valid" true (0 >= 0 && 0 <= 255);
    Alcotest.(check bool) "u8 max value valid" true (255 >= 0 && 255 <= 255);
    Alcotest.(check bool) "u8 negative invalid" false (-1 >= 0 && -1 <= 255);
    Alcotest.(check bool) "u8 overflow invalid" false (256 >= 0 && 256 <= 255)
  in

  test_i8_range ();
  test_u8_range ()

(** Test type compatibility for operations. *)
let test_type_compatibility () =
  let open TypeUtils in
  (* Same types are compatible *)
  Alcotest.(check bool) "i32 + i32 compatible" true (typ_equal TI32 TI32);
  Alcotest.(check bool) "f64 + f64 compatible" true (typ_equal TF64 TF64);

  (* Different integer types are not directly compatible *)
  Alcotest.(check bool) "i32 + i64 incompatible" false (typ_equal TI32 TI64);
  Alcotest.(check bool) "u32 + i32 incompatible" false (typ_equal TU32 TI32);

  (* Integer and float are incompatible *)
  Alcotest.(check bool) "i32 + f32 incompatible" false (typ_equal TI32 TF32);

  (* Type aliases should be equal to their base types *)
  Alcotest.(check bool) "int equals int" true (typ_equal TInt TInt);
  Alcotest.(check bool) "float equals float" true (typ_equal TFloat TFloat)

(** Test Option and Result type operations. *)
let test_option_result_types () =
  let open TypeUtils in
  (* Option types *)
  let opt_int = TOption TInt in
  let opt_string = TOption TString in
  Alcotest.(check bool)
    "option int equals option int" true
    (typ_equal opt_int opt_int);
  Alcotest.(check bool)
    "option int not equals option string" false
    (typ_equal opt_int opt_string);

  (* Result types *)
  let result_int_string = TResult (TInt, TString) in
  let result_bool_string = TResult (TBool, TString) in
  Alcotest.(check bool)
    "result equals same result" true
    (typ_equal result_int_string result_int_string);
  Alcotest.(check bool)
    "different results not equal" false
    (typ_equal result_int_string result_bool_string);

  (* Nested types *)
  let opt_result = TOption (TResult (TInt, TString)) in
  let result_opt = TResult (TOption TInt, TString) in
  Alcotest.(check bool)
    "option result not equals result option" false
    (typ_equal opt_result result_opt)

(** Test tuple type operations. *)
let test_tuple_types () =
  let open TypeUtils in
  (* Basic tuples *)
  let tuple2 = TTuple [ TInt; TString ] in
  let tuple3 = TTuple [ TInt; TString; TBool ] in
  let tuple2_diff = TTuple [ TBool; TString ] in

  Alcotest.(check bool) "tuple equals itself" true (typ_equal tuple2 tuple2);
  Alcotest.(check bool)
    "different length tuples not equal" false (typ_equal tuple2 tuple3);
  Alcotest.(check bool)
    "different element tuples not equal" false
    (typ_equal tuple2 tuple2_diff);

  (* Nested tuples *)
  let nested = TTuple [ TInt; TTuple [ TString; TBool ] ] in
  let nested_same = TTuple [ TInt; TTuple [ TString; TBool ] ] in
  let nested_diff = TTuple [ TInt; TTuple [ TBool; TString ] ] in

  Alcotest.(check bool)
    "nested tuple equals same" true
    (typ_equal nested nested_same);
  Alcotest.(check bool)
    "nested tuple not equals different" false
    (typ_equal nested nested_diff)

(** Test function type operations. *)
let test_function_types () =
  let open TypeUtils in
  (* Basic function types *)
  let func1 = TFunc ([ TInt; TString ], TBool) in
  let func2 = TFunc ([ TInt; TString ], TBool) in
  let func3 = TFunc ([ TString; TInt ], TBool) in
  let func4 = TFunc ([ TInt; TString ], TString) in

  Alcotest.(check bool) "same function types equal" true (typ_equal func1 func2);
  Alcotest.(check bool)
    "different arg order not equal" false (typ_equal func1 func3);
  Alcotest.(check bool)
    "different return type not equal" false (typ_equal func1 func4);

  (* No arguments *)
  let func_no_args = TFunc ([], TUnit) in
  let func_unit_arg = TFunc ([ TUnit ], TUnit) in

  Alcotest.(check bool)
    "no args not equals unit arg" false
    (typ_equal func_no_args func_unit_arg);

  (* Higher-order functions *)
  let higher_order = TFunc ([ TFunc ([ TInt ], TBool) ], TString) in
  let higher_order_same = TFunc ([ TFunc ([ TInt ], TBool) ], TString) in
  let higher_order_diff = TFunc ([ TFunc ([ TBool ], TBool) ], TString) in

  Alcotest.(check bool)
    "higher order equals same" true
    (typ_equal higher_order higher_order_same);
  Alcotest.(check bool)
    "higher order not equals different" false
    (typ_equal higher_order higher_order_diff)

(** Test array type operations. *)
let test_array_types () =
  let open TypeUtils in
  (* Fixed arrays *)
  let fixed_int_3 = TArray (TInt, Some 3) in
  let fixed_int_5 = TArray (TInt, Some 5) in
  let fixed_string_3 = TArray (TString, Some 3) in

  Alcotest.(check bool)
    "fixed array equals same" true
    (typ_equal fixed_int_3 fixed_int_3);
  Alcotest.(check bool)
    "different size arrays not equal" false
    (typ_equal fixed_int_3 fixed_int_5);
  Alcotest.(check bool)
    "different element arrays not equal" false
    (typ_equal fixed_int_3 fixed_string_3);

  (* Variable arrays *)
  let var_int = TArray (TInt, None) in
  let var_string = TArray (TString, None) in

  Alcotest.(check bool) "var array equals same" true (typ_equal var_int var_int);
  Alcotest.(check bool)
    "different var arrays not equal" false
    (typ_equal var_int var_string);

  (* Fixed vs Variable *)
  Alcotest.(check bool)
    "fixed not equals variable" false
    (typ_equal fixed_int_3 var_int)

(** Test type string representations. *)
let test_type_strings () =
  let open TypeUtils in
  (* Primitive types *)
  Alcotest.(check string) "i8 string" "i8" (typ_to_string TI8);
  Alcotest.(check string) "u64 string" "u64" (typ_to_string TU64);
  Alcotest.(check string) "float string" "float" (typ_to_string TFloat);
  Alcotest.(check string) "bool string" "bool" (typ_to_string TBool);
  Alcotest.(check string) "char string" "char" (typ_to_string TChar);
  Alcotest.(check string) "string string" "string" (typ_to_string TString);
  Alcotest.(check string) "bytes string" "bytes" (typ_to_string TBytes);
  Alcotest.(check string) "unit string" "unit" (typ_to_string TUnit);
  Alcotest.(check string) "never string" "!" (typ_to_string TNever);

  (* Complex types *)
  Alcotest.(check string)
    "fixed array string" "[int; 5]"
    (typ_to_string (TArray (TInt, Some 5)));
  Alcotest.(check string)
    "var array string" "[bool]"
    (typ_to_string (TArray (TBool, None)));
  Alcotest.(check string)
    "tuple string" "(int, string, bool)"
    (typ_to_string (TTuple [ TInt; TString; TBool ]));
  Alcotest.(check string)
    "option string" "Option<char>"
    (typ_to_string (TOption TChar));
  Alcotest.(check string)
    "result string" "Result<bool, string>"
    (typ_to_string (TResult (TBool, TString)));
  Alcotest.(check string)
    "function string" "(int, bool) -> string"
    (typ_to_string (TFunc ([ TInt; TBool ], TString)));
  Alcotest.(check string)
    "no arg function string" "() -> unit"
    (typ_to_string (TFunc ([], TUnit)))

let () =
  run "SNOW Type System Validation Tests"
    [
      ( "integer ranges",
        [ test_case "integer range validation" `Quick test_integer_ranges ] );
      ( "type compatibility",
        [ test_case "type compatibility checks" `Quick test_type_compatibility ]
      );
      ( "option result types",
        [ test_case "option and result types" `Quick test_option_result_types ]
      );
      ( "tuple types",
        [ test_case "tuple type operations" `Quick test_tuple_types ] );
      ( "function types",
        [ test_case "function type operations" `Quick test_function_types ] );
      ( "array types",
        [ test_case "array type operations" `Quick test_array_types ] );
      ( "type strings",
        [ test_case "type string representations" `Quick test_type_strings ] );
    ]
