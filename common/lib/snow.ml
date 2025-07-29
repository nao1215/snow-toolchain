(*** @project snow-toolchain @module Snow

  Main library interface for the SNOW programming language compiler.

  This module serves as the public API entry point for the SNOW common library.
  It provides version information and can be extended with additional
  library-wide constants, configuration, and utility functions.

  Used by: - Compiler executable: Version reporting and library identification -
  Build tools: Version-aware compilation and packaging - IDE integrations:
  Language server protocol implementations - Testing frameworks: Library version
  compatibility checks *)

(** Current version of the SNOW toolchain.

    Follows semantic versioning (semver) convention: MAJOR.MINOR.PATCH
    - MAJOR: Incompatible API changes
    - MINOR: Backwards-compatible functionality additions
    - PATCH: Backwards-compatible bug fixes

    Used for:
    - Compiler version reporting
    - Compatibility checking in build systems
    - Error messages and diagnostics *)
let version = "0.1.0"

module Ast = Ast
(** Re-export all public modules *)

module Lexer = Lexer
module Parser = Parser
module Typechecker = Typechecker
module Semantic = Semantic
module Analyzer = Analyzer
module Codegen = Codegen
module Formatter = Formatter
