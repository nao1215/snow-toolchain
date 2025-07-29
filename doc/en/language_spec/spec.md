# SNOW Programming Language Specification

Welcome to SNOW! This document provides a comprehensive specification for the SNOW programming language - a modern, functional programming language designed for safety, expressiveness, and performance.

## Table of Contents

1. [Type System](#type-system)
2. [Syntax and Language Paradigms](#syntax-and-language-paradigms)
3. [Control Structures and Functions](#control-structures-and-functions)
4. [Names and Scope](#names-and-scope)
5. [Package System and Type Definitions](#package-system-and-type-definitions)
6. [Expressions and Literals](#expressions-and-literals)
7. [Snow Command-Line Interface](#snow-command-line-interface)
8. [Getting Started](#getting-started)

---

## Type System

SNOW features a rich, static type system with explicit type annotations and Hindley-Milner type inference. The language prioritizes type safety and null safety through modern type system design.

### Primitive Types

#### Integer Types
SNOW provides explicit bit-width integer types for precise memory control:

- **Signed integers**: `i8`, `i16`, `i32`, `i64`
- **Unsigned integers**: `u8`, `u16`, `u32`, `u64`
- **Convenience aliases**: 
  - `int` (currently `i64`, may change in future versions)
  - `uint` (currently `u64`, may change in future versions)

```snow
let age: u8 = 25
let population: u64 = 7_800_000_000
let temperature: i32 = -15
```

#### Floating-Point Types
IEEE 754 compliant floating-point numbers:

- `f32`: 32-bit floating point
- `f64`: 64-bit floating point (default)
- `float`: Alias for `f64`

```snow
let pi: f64 = 3.14159
let precision: f32 = 1.5
let distance = 42.0  // Inferred as f64
```

#### Basic Types

- **`bool`**: Boolean values `true` and `false`
- **`char`**: Single Unicode scalar value (UTF-32)
- **`string`**: Immutable UTF-8 encoded text
- **`bytes`**: Arbitrary byte sequences for low-level processing
- **`unit`**: Void type, represented as `()`

```snow
let name: string = "Snow"
let initial: char = 'S'
let success: bool = true
let empty: unit = ()
```

### Advanced Types

#### Option Type
SNOW eliminates null pointer exceptions through the `Option<T>` type:

```snow
type Option<T> = union {
    Some(value: T);
    None
}

let maybeAge: Option<u8> = Some(25)
let noValue: Option<string> = None
```

#### Result Type  
Error handling without exceptions using `Result<T, E>`:

```snow
type Result<T, E> = union {
    Ok(value: T);
    Err(error: E)
}

let divide(a: f64, b: f64): Result<f64, string> =
    if b == 0.0 then
        Err("Division by zero")
    else
        Ok(a / b)
```

#### Arrays
- **Fixed-length arrays**: `[T; n]` (stack-allocated)
- **Variable-length arrays**: `[T]` (heap-allocated)

```snow
let coordinates: [f64; 3] = [1.0, 2.0, 3.0]  // Fixed-size
let numbers: [i32] = [1, 2, 3, 4, 5]         // Dynamic
```

#### Tuples
Heterogeneous data collections:

```snow
let point: (f64, f64) = (3.5, 2.1)
let person: (string, u8, bool) = ("Alice", 30, true)
```

#### Never Type
The `!` type represents computations that never return:

```snow
let panic(): ! = {
    print("Critical error!");
    exit(1)
}
```

### User-Defined Types

#### Structs
Named field collections with optional generic parameters:

```snow
type Point = struct {
    x: f64;
    y: f64
}

type Container<T> = struct {
    value: T;
    metadata: string
}
```

#### Algebraic Data Types (Union Types)
Sum types with pattern matching support:

```snow
type Shape = union {
    Circle(radius: f64);
    Rectangle(width: f64, height: f64);
    Triangle(a: f64, b: f64, c: f64)
}
```

#### Function Types
First-class functions with explicit type signatures:

```snow
type BinaryOp<T> = (T, T) -> T
type Predicate<T> = T -> bool
type Callback<T, R> = T -> R
```

---

## Syntax and Language Paradigms

SNOW combines functional programming principles with practical imperative features, emphasizing expression-based computation and type safety.

### Core Philosophy

#### Expression-Based Programming
In SNOW, everything is an expression that returns a value. There are no statements:

```snow
let result = if x > 0 then "positive" else "non-positive"

let value = match shape {
    Circle(r) -> pi * r * r;
    Rectangle(w, h) -> w * h;
    Triangle(a, b, c) -> {
        let s = (a + b + c) / 2.0;
        sqrt(s * (s - a) * (s - b) * (s - c))
    }
}
```

#### Functional Programming Foundation
- **Immutability by default**: Values cannot be changed unless explicitly marked `mutable`
- **Higher-order functions**: Functions as first-class values
- **Pure functions**: Emphasis on side-effect-free computation
- **Pattern matching**: Powerful destructuring and control flow

```snow
let numbers = [1, 2, 3, 4, 5]
let doubled = numbers.map(x -> x * 2)
let sum = numbers.reduce(0, (acc, x) -> acc + x)
```

#### Imperative Escape Hatches
When needed, SNOW provides controlled mutability:

```snow
let mutable counter = 0
counter := counter + 1

let mutable items: [string] = []
items.push("hello")
```

### Naming Conventions and Visibility

SNOW uses naming conventions to determine visibility, eliminating the need for explicit keywords:

#### Public Identifiers (CamelCase)
```snow
let PublicFunction(x: int): int = x * 2
type PublicType = struct { Value: int }
let PublicConstant: int = 42
```

#### Private Identifiers (camelCase)
```snow
let privateHelper(x: int): int = x + 1
let internalValue = "private"
```

#### Prohibited Naming Styles
- `snake_case` and `SCREAMING_SNAKE_CASE` are not allowed
- Constants use `CamelCase` instead of `SCREAMING_SNAKE_CASE`

---

## Control Structures and Functions

### Conditional Expressions

The `if` expression requires both `then` and `else` branches:

```snow
let absoluteValue(x: f64): f64 = 
    if x >= 0.0 then x else -x

let grade = if score >= 90 then "A"
           else if score >= 80 then "B"
           else if score >= 70 then "C"
           else "F"
```

### Pattern Matching

Powerful `match` expressions with exhaustiveness checking:

```snow
let processOption<T>(opt: Option<T>): string = match opt {
    Some(value) -> "Found: " + toString(value);
    None -> "No value"
}

let fibonacci(n: u32): u32 = match n {
    0 -> 0;
    1 -> 1;
    n -> fibonacci(n - 1) + fibonacci(n - 2)
}
```

### Function Definition and Application

#### Basic Function Definition
```snow
let greet(name: string, greeting: string): string =
    greeting + ", " + name + "!"
```

#### Labeled Arguments
Functions can be called with or without argument labels, but not mixed:

```snow
// With labels
let result1 = greet "Alice":name "Hello":greeting

// Without labels  
let result2 = greet "Bob" "Hi"

// Mixed style is PROHIBITED
// let result3 = greet "Charlie":name "Hey"  // Error!
```

#### Automatic Currying and Partial Application
All multi-argument functions are automatically curried:

```snow
let add(x: int, y: int): int = x + y
let addFive = add 5  // Partial application
let result = addFive 10  // Returns 15
```

#### Higher-Order Functions
Functions are first-class values:

```snow
let apply<T, R>(f: T -> R, value: T): R = f value
let compose<A, B, C>(f: B -> C, g: A -> B): A -> C = 
    x -> f(g(x))

let numbers = [1, 2, 3, 4, 5]
let squares = numbers.map(x -> x * x)
let evenSquares = squares.filter(x -> x % 2 == 0)
```

#### Closures
Functions capture their lexical environment:

```snow
let makeCounter(initial: int): () -> int = {
    let mutable count = initial;
    () -> {
        count := count + 1;
        count
    }
}

let counter = makeCounter(0)
let first = counter()   // Returns 1
let second = counter()  // Returns 2
```

---

## Names and Scope

SNOW uses lexical scoping with support for closures and controlled shadowing.

### Scope Types

#### Local Scope
Function parameters and `let` bindings create local scopes:

```snow
let outerFunction(x: int): int = {
    let y = x * 2;  // Local to outerFunction
    
    let innerFunction(z: int): int = {
        let w = z + 1;  // Local to innerFunction
        x + y + z + w   // Can access outer scopes
    };
    
    innerFunction(5)
}
```

#### Module Scope
File-level definitions with visibility based on naming:

```snow
// module.sw
let PublicFunction(): string = "accessible from other modules"
let privateFunction(): string = "only accessible within this module"
```

#### Shadowing
Inner scopes can shadow outer names:

```snow
let x = 10
let example(): int = {
    let x = 20;  // Shadows outer x
    {
        let x = 30;  // Shadows middle x
        x  // Returns 30
    }
}
```

---

## Package System and Type Definitions

SNOW adopts a Go-inspired package system with directory-based organization.

### Package Declaration

Every SNOW file must declare its package:

```snow
package math

let Sqrt(x: f64): f64 = // implementation
```

For executable programs:
```snow
package main

let main(): unit = {
    print("Hello, Snow!")
}
```

### Import System

```snow
import "math"
import "github.com/nao1215/snowlib/json"

let result = math.Sqrt(16.0)
let data = json.Parse("{\"name\": \"Snow\"}")
```

### Type Definitions

#### Type Aliases
Create nominal types for better API design:

```snow
type UserId = u64
type EmailAddress = string
type Temperature = f64

let sendEmail(to: EmailAddress, subject: string): Result<unit, string> = 
    // implementation
```

#### Struct Definitions
```snow
type Person = struct {
    Name: string;
    Age: u8;
    Email: EmailAddress
}

type Point3D<T> = struct {
    X: T;
    Y: T;
    Z: T
}
```

#### Union Type Definitions  
```snow
type HttpResponse<T> = union {
    Success(data: T, status: u16);
    ClientError(message: string, code: u16);
    ServerError(message: string, code: u16);
    NetworkError(reason: string)
}
```

### Visibility Rules

- **Uppercase identifiers**: Public (cross-package accessible)
- **Lowercase identifiers**: Private (package-local only)

---

## Expressions and Literals

### Literal Syntax

#### Numeric Literals
```snow
let integer = 42
let negative = -17
let large = 1_000_000  // Underscores for readability
let hex = 0xFF
let binary = 0b1010
let octal = 0o755

let pi = 3.14159
let scientific = 1.23e-4
let float32 = 2.5f32
```

#### String and Character Literals
```snow
let name = "Snow"
let greeting = "Hello, \"World\"!"
let multiline = """
    This is a
    multiline string
    with preserved indentation
"""
let character = 'S'
let unicode = '🌨'
```

#### Collection Literals
```snow
// Arrays
let numbers = [1, 2, 3, 4, 5]
let empty: [int] = []

// Tuples  
let point = (3.5, 2.1)
let triple = (1, "hello", true)

// Struct literals
let person = Person {
    Name = "Alice";
    Age = 30;
    Email = "alice@example.com"
}
```

### Operators and Precedence

Operator precedence from highest to lowest:

1. **Field access and indexing**: `.`, `[]`
2. **Multiplicative**: `*`, `/`, `%`
3. **Additive**: `+`, `-` (including string concatenation)
4. **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
5. **Logical AND**: `&&`
6. **Logical OR**: `||`
7. **Assignment**: `:=` (right-associative, mutable variables only)
8. **Pipe**: `|>` (left-associative)

```snow
let result = numbers
    |> filter(x -> x > 0)
    |> map(x -> x * 2)
    |> reduce(0, (a, b) -> a + b)

let mutable counter = 0
counter := counter + 1
```

### Expression Types

#### Block Expressions
```snow
let complexCalculation = {
    let a = 10;
    let b = 20;
    let sum = a + b;
    sum * 2  // Last expression is returned
}
```

#### Let Expressions
```snow
let result = 
    let x = compute_x() in
    let y = compute_y() in
    x + y
```

---

## Snow Command-Line Interface

The `snow` command provides a comprehensive toolchain for SNOW development.

### Core Commands

#### Building and Running
```bash
snow build main.sw              # Compile to executable
snow build -o hello main.sw     # Specify output name
snow build -O2 -g main.sw       # Optimized build with debug info

snow run main.sw                # Compile and run immediately  
snow run main.sw -- arg1 arg2   # Pass arguments to program
snow run -v main.sw             # Verbose output
```

#### Testing and Formatting
```bash
snow test                       # Run all *_test.sw files
snow test package_test.sw       # Run specific test file

snow fmt                        # Format all .sw files
snow fmt main.sw util.sw        # Format specific files
```

#### Interactive Development
```bash
snow repl                       # Launch REPL (planned)
```

### Package Management

#### Project Initialization
```bash
snow mod init                   # Create snow.mod file
snow mod init github.com/user/project  # With module path
```

#### Dependency Management  
```bash
snow mod tidy                   # Clean and update dependencies
snow mod download               # Download all dependencies
snow get github.com/user/lib    # Add new dependency
snow install                    # Install tools globally
```

#### Configuration Files

**snow.mod** (YAML format):
```yaml
module: github.com/user/myproject
version: 1.0.0

dependencies:
  github.com/nao1215/snowlib: v1.2.3
  github.com/other/package: v2.1.0

dev-dependencies:
  github.com/testing/framework: v1.0.0
```

---

## Getting Started

### Installation

1. **Prerequisites**: Ensure you have OCaml, Dune, and LLVM installed
2. **Build from source**:
   ```bash
   git clone https://github.com/nao1215/snow-toolchain
   cd snow-toolchain
   eval $(opam env)
   dune build
   dune install
   ```

### Your First SNOW Program

Create `hello.sw`:
```snow
package main

let main(): unit = {
    print("Hello, Snow! ❄️")
}
```

Run it:
```bash
snow run hello.sw
# Output: Hello, Snow! ❄️
```

### Example Programs

#### Calculator
```snow
package main

type Operation = union {
    Add(f64, f64);
    Subtract(f64, f64);
    Multiply(f64, f64);
    Divide(f64, f64)
}

let calculate(op: Operation): Result<f64, string> = match op {
    Add(a, b) -> Ok(a + b);
    Subtract(a, b) -> Ok(a - b);
    Multiply(a, b) -> Ok(a * b);
    Divide(a, b) -> if b == 0.0 then 
                        Err("Division by zero") 
                    else 
                        Ok(a / b)
}

let main(): unit = {
    let result = calculate(Divide(10.0, 2.0));
    match result {
        Ok(value) -> print("Result: " + toString(value));
        Err(error) -> print("Error: " + error)
    }
}
```

#### List Processing
```snow
package main

let processNumbers(numbers: [i32]): [i32] = 
    numbers
    |> filter(x -> x > 0)
    |> map(x -> x * x)
    |> sort

let main(): unit = {
    let input = [3, -1, 4, -2, 5, 0, 1];
    let processed = processNumbers(input);
    
    print("Original: " + toString(input));
    print("Processed: " + toString(processed))
}
```

---

## Language Status

SNOW is currently under active development. The implementation includes:

✅ **Completed**:
- Lexical analysis and parsing
- Type system definitions  
- Expression evaluation
- Basic code generation (LLVM)
- Core toolchain (`build`, `run`, `fmt`)

🚧 **In Progress**:
- Advanced type checker
- Complete pattern matching
- Module system
- Standard library

📋 **Planned**:
- REPL implementation
- Package management
- IDE integration
- Performance optimizations

---

*This specification describes the SNOW programming language as designed. Some features may not be fully implemented yet. Check the project repository for current implementation status.*

**Repository**: https://github.com/nao1215/snow-toolchain  
**License**: MIT
