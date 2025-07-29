# Minimal FFI Specification for Snow

This document presents a pragmatic, incremental approach to FFI implementation for Snow, designed to be easily integrated with the current codebase while providing essential functionality for standard library development.

## Design Principles

1. **Minimal Changes** - Reuse existing infrastructure where possible
2. **Incremental Implementation** - Each phase builds on the previous one
3. **Immediate Usability** - Each phase provides useful functionality
4. **Safety by Default** - Unsafe operations are clearly marked but not overly restrictive

## Phase 1: Basic External Functions (Minimal MVP)

### 1.1 External Function Declaration

Since the codegen already uses `declare_function` for printf, we can expose this at the language level with minimal changes:

```snow
// Simple syntax - just add 'extern' keyword to existing function syntax
extern fn puts(s: string) -> i32
extern fn exit(code: i32) -> unit
extern fn malloc(size: i64) -> i64  // Initially use i64 for pointers
```

**Implementation Notes:**
- Reuse existing `func` AST node with an `is_extern: bool` field
- In codegen, if `is_extern` is true, use `declare_function` without body
- No new AST nodes needed initially

### 1.2 String Handling (Leverage Existing)

The current implementation already converts Snow strings to C strings in codegen:
```ocaml
| EString s -> build_global_stringptr s "str" ctx.builder
```

We can use this directly:

```snow
extern fn puts(s: string) -> i32

fn main() {
    puts("Hello, World!")  // Works with existing string handling
}
```

**Implementation Notes:**
- No changes needed for basic string passing
- The existing `build_global_stringptr` handles null-termination

### 1.3 Integer Pointer Workaround

Initially, use `i64` for pointers (common on 64-bit systems):

```snow
extern fn malloc(size: i64) -> i64
extern fn free(ptr: i64) -> unit

fn allocate_int() -> i64 {
    let ptr = malloc(8)  // 8 bytes for i64
    ptr
}
```

**Implementation Notes:**
- No new types needed initially
- Can cast between i64 and LLVM pointer types in codegen

## Phase 2: Minimal Unsafe Blocks

### 2.1 Simple Unsafe Expression

Add a simple `unsafe` expression rather than a full block syntax:

```snow
// AST addition:
// | EUnsafe of expr

let result = unsafe(call_c_function(args))
```

**Implementation Notes:**
- Single new AST node: `EUnsafe of expr`
- Type checker passes through the inner expression's type
- Codegen just evaluates the inner expression (marker for future checks)

### 2.2 Built-in Memory Functions

Provide built-in functions for common pointer operations:

```snow
// Built-in functions (implemented in codegen, not as external)
fn ptr_read(addr: i64) -> i64 { unsafe(...) }
fn ptr_write(addr: i64, value: i64) -> unit { unsafe(...) }
fn ptr_offset(addr: i64, offset: i64) -> i64 { unsafe(...) }
```

**Implementation Notes:**
- Implement as special cases in codegen
- Use LLVM's load/store/GEP instructions directly

## Phase 3: Basic Pointer Types

### 3.1 Simplified Pointer Type

Add a single generic pointer type initially:

```snow
// AST addition:
// | TPtr  // Generic pointer type

type ptr = ptr  // Built-in pointer type

extern fn malloc(size: i64) -> ptr
extern fn free(p: ptr) -> unit
```

**Implementation Notes:**
- Add `TPtr` to the `typ` enum
- In LLVM, map to `i8*` (generic byte pointer)
- Add built-in conversion functions:

```snow
fn ptr_to_i64(p: ptr) -> i64 { ... }
fn i64_to_ptr(i: i64) -> ptr { ... }
```

### 3.2 Basic Pointer Operations

```snow
// Dereferencing with type hints
fn read_i32(p: ptr) -> i32 {
    unsafe(ptr_read_i32(p))  // Built-in type-specific read
}

fn write_i32(p: ptr, val: i32) -> unit {
    unsafe(ptr_write_i32(p, val))
}
```

## Phase 4: Variadic Functions

### 4.1 Simple Printf Support

Special-case printf-like functions initially:

```snow
// Special syntax for known variadic functions
extern fn printf(format: string, ...) -> i32

fn main() {
    printf("Hello %s, age %d\n", "Alice", 25)
}
```

**Implementation Notes:**
- Parser recognizes `...` in extern functions only
- Codegen already handles variadic calls for printf
- Type checking is relaxed for variadic arguments

## Implementation Roadmap

### Immediate (Phase 1) - 1-2 days
1. Add `is_extern` field to function AST
2. Update parser to recognize `extern fn`
3. Modify codegen to skip body generation for extern functions
4. Test with puts, exit, malloc

### Short-term (Phase 2) - 2-3 days
1. Add `EUnsafe` expression node
2. Implement built-in pointer operations
3. Add basic safety warnings

### Medium-term (Phase 3) - 3-5 days
1. Add `TPtr` type
2. Implement pointer/integer conversions
3. Add type-specific read/write functions

### Long-term (Phase 4) - 1 week
1. Add variadic function support
2. Implement printf properly
3. Add more standard library functions

## Example: Minimal Standard Library

With just Phase 1, we can implement:

```snow
// std/io.sw
extern fn puts(s: string) -> i32
extern fn putchar(c: i32) -> i32

fn print(s: string) {
    puts(s)
}

fn println(s: string) {
    print(s)
    putchar(10)  // newline
}
```

```snow
// std/sys.sw
extern fn exit(code: i32) -> unit
extern fn getpid() -> i32
extern fn sleep(seconds: i32) -> i32

fn terminate(code: i32) {
    exit(code)
}
```

## Comparison with Original Specification

### Simplifications:
1. **No pointer type syntax initially** - Use i64/ptr instead of `*const T`
2. **No unsafe blocks** - Just unsafe expressions
3. **No C string literals** - Use existing string conversion
4. **No complex type mappings** - Start with basic types only
5. **No memory management complexity** - Manual with simple functions

### Advantages:
1. **Faster implementation** - Can start using FFI in days, not weeks
2. **Less parser changes** - Reuse existing function syntax
3. **Incremental testing** - Each phase is independently useful
4. **Lower risk** - Smaller changes to existing code

### Migration Path:
- Code written for the minimal spec will work with the full spec
- Can gradually add features based on actual needs
- Early feedback from standard library implementation

## Testing Strategy

### Phase 1 Tests:
```snow
// test_extern.sw
extern fn puts(s: string) -> i32

fn test_puts() {
    let result = puts("Hello from Snow!")
    assert(result >= 0)
}
```

### Phase 2 Tests:
```snow
// test_unsafe.sw
extern fn malloc(size: i64) -> i64
extern fn free(ptr: i64) -> unit

fn test_malloc() {
    let ptr = unsafe(malloc(100))
    assert(ptr != 0)
    unsafe(free(ptr))
}
```

## Safety Considerations

Even with this minimal approach, we maintain safety:

1. **Explicit unsafe marking** - All dangerous operations require `unsafe`
2. **Type checking** - Basic type safety for non-variadic functions
3. **Documentation** - Clear warnings about pointer operations
4. **Gradual enhancement** - Can add more safety features later

## Conclusion

This minimal specification provides:
- **Quick wins** - Usable FFI in days
- **Low risk** - Small, incremental changes
- **Real utility** - Enough to build basic standard library
- **Future-proof** - Compatible with full specification later

The key insight is that we don't need a perfect FFI system to start building useful functionality. We can iterate based on real needs rather than theoretical requirements.
