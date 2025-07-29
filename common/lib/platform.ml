(*** @project snow-toolchain @module Platform

  Cross-platform utility functions for Snow compiler.

  Provides platform detection and platform-specific configuration for FFI
  functions and system calls. *)

(** Platform types supported by Snow *)
type platform = Linux | Windows | MacOS | BSD | Unknown

(** Architecture types *)
type arch = X86_64 | X86_32 | ARM64 | ARM32 | Unknown_Arch

type platform_info = {
  platform : platform;
  arch : arch;
  pointer_size : int; (* in bytes *)
  libc : string; (* C library name *)
}
(** Platform information record *)

(** Get current platform from environment *)
let detect_platform () =
  let os = try Sys.getenv "OS" with Not_found -> "" in
  let uname =
    try
      let ic = Unix.open_process_in "uname -s" in
      let result = input_line ic in
      ignore (Unix.close_process_in ic);
      result
    with _ -> ""
  in
  match (String.lowercase_ascii uname, String.lowercase_ascii os) with
  | "linux", _ -> Linux
  | "darwin", _ -> MacOS
  | _, "windows_nt" | "cygwin", _ | "mingw", _ -> Windows
  | s, _ when Str.string_match (Str.regexp ".*bsd.*") s 0 -> BSD
  | _ -> Unknown

(** Get current architecture *)
let detect_arch () =
  let arch_str =
    try
      let ic = Unix.open_process_in "uname -m" in
      let result = input_line ic in
      ignore (Unix.close_process_in ic);
      result
    with _ -> ""
  in
  match String.lowercase_ascii arch_str with
  | "x86_64" | "amd64" -> X86_64
  | "i386" | "i686" -> X86_32
  | "arm64" | "aarch64" -> ARM64
  | "arm" | "armv7l" -> ARM32
  | _ -> Unknown_Arch

(** Get platform information *)
let get_platform_info () =
  let platform = detect_platform () in
  let arch = detect_arch () in
  let pointer_size =
    match arch with
    | X86_64 | ARM64 -> 8
    | X86_32 | ARM32 -> 4
    | Unknown_Arch -> 8 (* default to 64-bit *)
  in
  let libc =
    match platform with
    | Linux | BSD -> "libc"
    | MacOS -> "libSystem"
    | Windows -> "msvcrt"
    | Unknown -> "libc"
  in
  { platform; arch; pointer_size; libc }

(** Platform-specific function mappings *)
module FunctionMap = struct
  (** Memory allocation functions *)
  let malloc_function = function
    | Linux | MacOS | BSD | Unknown -> "malloc"
    | Windows -> "malloc" (* Available in msvcrt *)

  let free_function = function
    | Linux | MacOS | BSD | Unknown -> "free"
    | Windows -> "free" (* Available in msvcrt *)

  (** Standard I/O functions *)
  let printf_function = function
    | Linux | MacOS | BSD | Unknown -> "printf"
    | Windows -> "printf" (* Available in msvcrt *)

  let puts_function = function
    | Linux | MacOS | BSD | Unknown -> "puts"
    | Windows -> "puts" (* Available in msvcrt *)

  let exit_function = function
    | Linux | MacOS | BSD | Unknown -> "exit"
    | Windows -> "exit" (* Available in msvcrt *)

  (** Get platform-specific function name *)
  let get_function_name platform func_type =
    match func_type with
    | "malloc" -> malloc_function platform
    | "free" -> free_function platform
    | "printf" -> printf_function platform
    | "puts" -> puts_function platform
    | "exit" -> exit_function platform
    | name -> name (* Return original name for unknown functions *)
end

(** Platform-specific type sizes *)
module TypeSizes = struct
  let pointer_size platform_info = platform_info.pointer_size

  let size_t_size = function
    | { arch = X86_64 | ARM64; _ } -> 8
    | { arch = X86_32 | ARM32; _ } -> 4
    | { arch = Unknown_Arch; _ } -> 8

  let int_size = function
    | { platform = Windows; _ } -> 4 (* int is always 32-bit on Windows *)
    | _ -> 4 (* int is typically 32-bit on most platforms *)

  let long_size = function
    | { platform = Windows; arch = X86_64 | ARM64; _ } ->
        4 (* long is 32-bit on Win64 *)
    | { arch = X86_64 | ARM64; _ } -> 8 (* long is 64-bit on 64-bit Unix *)
    | _ -> 4 (* long is 32-bit on 32-bit systems *)
end

(** Platform-specific linking options *)
module LinkOptions = struct
  let get_link_libraries = function
    | Linux -> [ "-lc" ]
    | MacOS -> [] (* libSystem is linked by default *)
    | BSD -> [ "-lc" ]
    | Windows -> [ "-lmsvcrt" ]
    | Unknown -> [ "-lc" ]

  let get_link_flags = function
    | Windows -> [ "-Wl,--subsystem,console" ]
    | _ -> []
end

(** Current platform info (computed once) *)
let current_platform = lazy (get_platform_info ())
