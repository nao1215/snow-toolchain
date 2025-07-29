# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. Claude Code responses answers in Japanese, however, the code itself should be in English.

## Project Overview

Snow-toolchain is a compiler and toolchain for the SNOW programming language, implemented in OCaml using the Dune build system. SNOW is a modern functional programming language that combines:

- **Go-inspired ecosystem**: Official comprehensive toolchain and package management system
- **OCaml-style functional programming**: Expression-based syntax, rich type system, and advanced type inference
- **Type and null safety**: Option/Result types, static typing, and comprehensive error handling
- **LLVM backend**: Native code generation for high performance

**Project Philosophy**: We aim to provide the same level of official tooling and ecosystem support that makes Go productive, while offering the expressiveness and safety of OCaml-family functional programming languages.

## Build and Development Commands

If you does not find the `dune` command, you may need to execute `eval $(opam env)` to set up the environment variables for the current shell session.

```bash

dune build                    # Build entire project
```

### Running Tests
```bash
dune test                     # Run all tests
```

### Compiler Usage
```bash
dune exec snow -- build <source.sw>   # Compile SNOW source file
dune exec snow -- run <source.sw>     # Compile and run immediately
dune exec snow -- fmt                 # Format source code
dune exec snow -- mod init            # Initialize module with snow.mod
```

### File Extension
SNOW source files use the `.sw` extension (e.g., `main.sw`, `hello.sw`)

## Testing Requirements

**CRITICAL**: This project requires comprehensive test coverage for all new features and modifications. When implementing any new functionality:

1. **Write tests first (TDD approach recommended)**
2. **All new code MUST have corresponding test cases**
3. **Tests must pass before submitting changes**: `dune runtest`
4. **Test naming convention**: Use descriptive test names that explain the expected behavior
5. **Test coverage areas**:
   - Lexer tests for new token types
   - Parser tests for new syntax constructs  
   - Type checker tests for type safety
   - Codegen tests for LLVM IR generation
   - Integration tests for end-to-end compilation

Use the Alcotest framework following existing patterns in `compiler/test/`. The test suite currently includes 51+ comprehensive test cases covering all language features.

## Architecture Overview

The codebase follows a standard compiler architecture with clear separation of concerns:

### Core Components

**common/lib/** - Shared compiler frontend components:
- `ast.ml` - Abstract Syntax Tree definitions for all SNOW language constructs
- `lexer.ml` - Lexical analyzer that tokenizes SNOW source code 
- `parser.ml` - Recursive descent parser that builds AST from token streams
- `typechecker.ml` - Basic type checker implementation
- `advanced_typechecker.ml` - Advanced type inference with Hindley-Milner
- `semantic.ml` - Semantic analysis and validation
- `codegen.ml` - LLVM code generation
- `formatter.ml` - Code formatting and pretty-printing
- `snow.ml` - Main library interface

**compiler/** - Compiler-specific implementations:
- `bin/snow/` - Main compiler executable with comprehensive subcommand support
  - `main.ml` - Entry point and command dispatch
  - `run.ml` - `snow run` command implementation (compile and execute)
  - `mod_init.ml` - `snow mod init` command for Go-style module initialization
- `test/` - Comprehensive test suite with Alcotest framework (51+ test cases)

### Language Design

SNOW is designed as an expression-based functional language with:

- **Rich type system**: Explicit bit-width integers (i8, u8, i16, etc.), IEEE 754 floats, Option/Result types
- **Expression-oriented**: Most constructs return values rather than performing statements  
- **Go-inspired package system**: Directory-based modules with `package` declarations and `snow.mod` files
- **LLVM backend**: Code generation targets LLVM IR for native performance
- **Type and null safety**: Option/Result types eliminate null pointer exceptions
- **Functional paradigms**: First-class functions, automatic currying, immutable data structures
- **Comprehensive toolchain**: Integrated build, run, test, format, and package management

### Compilation Pipeline

1. **Lexer** (`lexer.ml`) - Converts source text to token stream
2. **Parser** (`parser.ml`) - Builds AST using recursive descent with operator precedence
3. **Semantic Analysis** (`semantic.ml`) - Variable scope and naming convention validation
4. **Type Checker** (`typechecker.ml`, `advanced_typechecker.ml`) - Type inference and validation
5. **Code Generator** (`codegen.ml`) - Emits LLVM IR from typed AST

### Key Implementation Details

- **Parser Strategy**: Recursive descent with precedence climbing for expressions
- **AST Design**: Immutable functional data structures with comprehensive type information
- **Token Sharing**: Common token definitions across lexer, parser, and tooling
- **Error Handling**: Result types for safe error propagation throughout pipeline
- **Testing**: Comprehensive test coverage with 48+ test cases across all components

### Current Development Status

**Implemented (Production Ready):**
- Complete type system definitions with utility functions
- Full lexical analysis supporting all SNOW syntax
- Expression parsing with operator precedence and control structures
- Pattern matching and function definitions
- Basic and advanced type checking with Hindley-Milner inference  
- LLVM code generation for core language constructs
- Semantic analysis and validation
- Code formatting and pretty-printing
- Comprehensive toolchain:
  - `snow build` - Compile to executable
  - `snow run` - Compile and execute immediately
  - `snow fmt` - Code formatting
  - `snow mod init` - Go-style module initialization
- `snow mod tidy` - Dependency management
- `snow install` - Remote package installation
- Complete test suite (51+ test cases, all passing)

**In Progress:**
- Package resolution and imports
- Standard library implementation

**Planned:**
- `snow test` - Test runner for `*_test.sw` files
- `snow repl` - Interactive REPL
- IDE integration and Language Server Protocol
- Performance optimizations
- Additional LLVM optimization passes

### Language Specifications

Detailed language specifications are available in:
- **English**: `doc/en/language_spec/spec.md` (comprehensive language guide for beginners)

Covering:
- Type system and primitive types
- Syntax and paradigms  
- Control structures and functions
- Names and scope
- Packages and type definitions
- Expressions and literals
- Snow command CLI and toolchain

**Development Guidelines:**
1. **Maintain expression-based language design** - Everything should return a value
2. **Ensure comprehensive test coverage** - All new features must have tests
3. **Follow functional programming patterns** - Immutable data structures, pure functions
4. **Prioritize type and null safety** - Leverage Option/Result types
5. **Test-driven development** - Write tests before implementing features
6. **Go-style ecosystem approach** - Official tooling should be comprehensive and integrated

## snow command

| Subcommand Name     | Status | Description                                                   |
| ------------------- | ------ | ------------------------------------------------------------- |
| `snow build`        | ✅ Ready | Builds the project and generates an executable |
| `snow run`          | ✅ Ready | Compile and execute a Snow program immediately (like `go run`) |
| `snow fmt`          | ✅ Ready | Formats the source code using official style guidelines |
| `snow mod init`     | ✅ Ready | Initializes a module with `snow.mod` (like `go mod init`) |
| `snow mod tidy`     | ✅ Ready | Removes unused dependencies and adds missing ones |
| `snow mod download` | 🚧 Planned | Downloads dependency packages |
| `snow install`      | ✅ Ready | Downloads, compiles and installs remote packages with main package (like `go install`) |
| `snow test`         | 🚧 Planned | Runs test code (`*_test.sw`) |
| `snow repl`         | 🚧 Planned | Launches an interactive REPL |
| `snow version`      | ✅ Ready | Displays the current compiler and runtime version |

### Usage Examples

```bash
# Create a new SNOW module
snow mod init
snow mod init github.com/user/myproject

# Compile and run immediately  
snow run main.sw
snow run main.sw -- arg1 arg2  # Pass arguments

# Build to executable
snow build main.sw
snow build -o hello main.sw

# Format code
snow fmt                # Format all .sw files
snow fmt main.sw util.sw  # Format specific files

# Install remote packages
snow install github.com/user/tool@v1.2.3    # Install specific version
snow install github.com/user/tool@latest    # Install latest version
snow install github.com/user/tool           # Install latest version (default)
```

## Snow Install Command Specification

The `snow install` command downloads, compiles, and installs remote Snow packages that contain a main package, similar to `go install`. This enables users to easily install Snow-based command-line tools and executables from remote repositories.

### Command Syntax

```bash
snow install <module-path>[@version]
```

- `<module-path>`: Git repository path (e.g., `github.com/user/tool`)
- `@version` (optional): Specific version tag, branch, or commit
  - `@v1.2.3`: Specific git tag version
  - `@latest`: Latest git tag (default if omitted)
  - `@main`: Specific branch
  - `@commit-hash`: Specific commit

### Directory Structure and File Placement

Following Go's conventions, Snow uses environment variables and standard directories for package management:

#### Environment Variables

- `SNOWPATH`: Snow workspace directory (defaults to `$HOME/snow`)
- `SNOWBIN`: Directory for installed executables (defaults to `$SNOWPATH/bin`)

#### Source Code Placement

Downloaded source code is cached in module cache directories:

```
$SNOWPATH/pkg/mod/
├── github.com/
│   └── user/
│       └── tool@v1.2.3/     # Versioned module cache
│           ├── snow.mod
│           ├── main.sw      # Main package
│           └── src/
└── example.com/
    └── org/
        └── project@latest/  # Latest version cache
```

#### Executable Placement

Compiled executables are installed to the binary directory:

```
$SNOWPATH/bin/
├── tool         # Executable from github.com/user/tool
├── myapp        # Executable from example.com/org/myapp
└── snow-format  # Executable with hyphenated name
```

The executable name is derived from the last component of the module path or from the binary name specified in `snow.mod`.

### Installation Process

1. **Parse Arguments**: Extract module path and version specification
2. **Resolve Version**: Determine the exact git tag/commit to fetch
3. **Check Cache**: Look for existing cached version in `$SNOWPATH/pkg/mod/`
4. **Download Source**: Clone git repository to cache directory if not present
5. **Validate Main Package**: Verify the presence of main package in downloaded source
6. **Compile**: Build the main package to create executable binary
7. **Install Binary**: Copy executable to `$SNOWPATH/bin/`
8. **Update PATH**: Ensure `$SNOWPATH/bin` is in user's PATH

### snow.mod Integration

If the target repository contains a `snow.mod` file, the install command will:

- Read binary name from metadata (if specified)
- Install dependencies required for compilation  
- Respect minimum Snow version requirements

Example `snow.mod` with binary metadata:

```yaml
module: github.com/user/tool
version: v1.2.3
license: MIT

# Optional: specify custom binary name
metadata:
  bin-name: "custom-tool-name"
  min-snow-version: "0.1.0"
```

### Command Options

```bash
snow install [OPTIONS] <module-path>[@version]

OPTIONS:
  -f, --force       Force reinstallation even if already installed
  -v, --verbose     Verbose output showing download and build progress
  --no-cache        Skip cache and always download fresh copy
```

### Error Handling

The install command handles various failure scenarios:

- **Repository not found**: Clear error message with repository URL
- **No main package**: Error if downloaded code lacks main package
- **Version not found**: Error for invalid git tags or branches  
- **Compilation failure**: Show compilation errors with file context
- **Permission errors**: Guide user to fix directory permissions
- **Network errors**: Retry logic with timeout for git operations

### Examples

```bash
# Install latest version of a tool
snow install github.com/user/snow-formatter

# Install specific version
snow install github.com/user/snow-lint@v2.1.0

# Install from specific branch
snow install github.com/user/experimental-tool@develop

# Force reinstall with verbose output
snow install -f -v github.com/user/tool@v1.0.0
```

### Integration with PATH

After installation, users should ensure `$SNOWPATH/bin` is in their PATH:

```bash
# Add to ~/.bashrc or ~/.zshrc
export SNOWPATH=$HOME/snow
export PATH=$SNOWPATH/bin:$PATH
```

The installer should provide guidance on PATH configuration if binaries are installed but not accessible.

## Mandatory Testing Protocol

**ABSOLUTE REQUIREMENT**: Every single code change must include comprehensive tests. This is non-negotiable.

### Before Making Any Changes:
1. Run `dune runtest` to ensure current tests pass
2. Identify what tests need to be added for your changes
3. Write failing tests that describe the expected behavior

### During Development:
1. Follow Test-Driven Development (TDD) approach
2. Write tests before writing implementation code
3. Ensure tests fail initially (red)
4. Implement code to make tests pass (green)
5. Refactor while keeping tests green

### Before Completing Work:
1. All tests must pass: `dune runtest` shows no failures
2. New functionality must have corresponding test cases
3. Edge cases and error conditions must be tested
4. Test names must clearly describe what they verify

### Test Categories Required:
- **Lexer tests**: For any new tokens or lexical constructs
- **Parser tests**: For any new syntax or language features  
- **Type tests**: For type checking and inference behavior
- **Codegen tests**: For LLVM IR generation correctness
- **Integration tests**: For end-to-end compiler behavior
- **CLI tests**: For new command-line functionality

**Remember**: The test suite is currently at 51+ comprehensive test cases. Any addition to the language or toolchain must maintain this high standard of test coverage.
