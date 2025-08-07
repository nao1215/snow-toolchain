# ❄️ Snow Programming Language

Welcome to **Snow** - an experimental functional programming language that combines Go-inspired tooling with OCaml-style functional programming. 

⚠️ **Status**: Snow is currently in early development. Many features are experimental or not yet implemented.

## What is Snow?

Snow aims to provide Go-like ecosystem productivity with functional programming expressiveness and safety. The language is being developed as a research project to explore modern compiler design and functional language features.

### Planned Features

- **Type & Null Safety**: Option/Result types for safer error handling
- **Type Inference**: Hindley-Milner style type inference
- **Expression-Based**: Functional programming with expression-oriented syntax
- **LLVM Backend**: Native code generation through LLVM
- **Go-Style Tooling**: Integrated build system and package management
- **Functional Paradigms**: Immutable data structures and pattern matching
- **Standard Library**: https://github.com/nao1215/snow

## Quick Start 🏁

### Installation

```bash
# Build the Snow toolchain
dune build

# Add to your shell profile
export PATH=$PATH:$(pwd)/_build/default/compiler/bin/snow
```

### Your First Snow Program

Create a `hello.sw` file:

```snow
package main

// Snow loves expressions and safety!
let main(): unit = {
	let greeting = "Hello, Snow! ❄️" in
	println(greeting)
}
```

Run it:

```bash
snow run hello.sw
```

## The Snow Toolchain 🔧

Current implementation status of Snow toolchain commands:

| Command | Status | Description |
|---------|--------|-------------|
| `snow build` | ✅ Working | Compile Snow programs to native executables |
| `snow run` | ✅ Working | Compile and run immediately |
| `snow fmt` | ✅ Working | Format code with official style |
| `snow mod init` | ✅ Working | Initialize a new Snow module |
| `snow mod tidy` | 🚧 Partial | Manage dependencies (basic implementation, missing network validation) |
| `snow install` | ✅ Working | Install tools from remote repositories |
| `snow test` | 🚧 Planned | Run test suite |
| `snow repl` | 🚧 Planned | Interactive REPL |

### Examples

```bash
# Start a new project
snow mod init github.com/yourname/awesome-project

# Install a Snow-based tool from GitHub
snow install github.com/nao1215/snow/sample/hello

# Build with optimizations (basic LLVM-level optimizations)
snow build -O2 -o myapp src/main.sw

# Format all Snow files in your project
snow fmt **/*.sw
```

## Package Management 📦

Snow uses a Go-inspired package system with `snow.mod` files and GitHub-based dependencies:

```yaml
# Snow module file
# This file describes the module and its dependencies

module: "github.com/nao1215/snow"
version: "v0.1.0"
license: MIT

# Dependencies (will be populated by 'snow mod tidy')
dependencies: {}

# Development dependencies (for testing, etc.)
dev-dependencies: {}

# Module metadata
metadata:
  min-snow-version: "0.1.0"
  build-system: "snow"
```

## Current Language Features 

**Implemented:**
- Basic type system with explicit integer types (`i8`, `u8`, `i32`, etc.)
- Lexical analysis and parsing
- Type inference using Hindley-Milner algorithm
- LLVM code generation for core language constructs
- Pattern matching for basic types
- Function definitions and calls

**In Development:**
- Advanced pattern matching
- Module system and imports
- Standard library
- Error handling with Result types

## Documentation 📖

- **Language Specification**: For complete language details, see [`doc/en/language_spec/spec.md`](doc/en/language_spec/spec.md)

## Development 🛠️

Want to contribute? Awesome! Here's how to get started:

```bash
# Clone and build
git clone https://github.com/nao1215/snow-toolchain.git
cd snow-toolchain
dune build

# Run the test suite
dune runtest

# Try the compiler
dune exec snow -- --help
```

### Project Structure

- `common/lib/` - Core compiler components (lexer, parser, typechecker, codegen)
- `compiler/bin/snow/` - The main Snow toolchain CLI
- `compiler/test/` - Comprehensive test suite (50+ tests!)
- `doc/` - Language documentation in English and Japanese

## Project Goals

This project explores:
- Safe functional programming language design
- Modern compiler implementation techniques
- Integration of type inference with practical tooling
- LLVM backend code generation

The aim is to research how functional programming concepts can be made more accessible through familiar tooling patterns.

## Community 🤝

- **Issues & Discussion**: [GitHub Issues](https://github.com/nao1215/snow-toolchain/issues)
- **Contributions**: We welcome PRs! Please check our contribution guidelines
- **Support**: https://github.com/sponsors/nao1215

## License 📄

This project is licensed under the terms specified in the repository.

---

**Interested in functional language research?** 

Check out the [language specification](doc/en/language_spec/spec.md) for implementation details, or try `dune exec snow -- --help` to explore the current toolchain.
