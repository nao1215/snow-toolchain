# Snow Dependency Management

This document describes the dependency management system in Snow, which closely follows Go's module system design but uses YAML format for configuration files.

## Overview

Snow uses two key files for dependency management:
- `snow.mod` - Module definition and dependency declarations (YAML format)
- `snow.sum` - Cryptographic checksums for dependency verification (text format)

The system provides reproducible builds, secure dependency verification, and automatic dependency resolution similar to Go modules.

## snow.mod File Specification

The `snow.mod` file defines a Snow module and its dependencies. It uses YAML format for better readability while maintaining the same semantics as Go's `go.mod`.

### Basic Structure

```yaml
# Snow module file
module: github.com/user/myproject
snow: "0.1.0"  # Minimum required Snow version

# Direct dependencies only
require:
  github.com/user/lib1: v1.2.3
  github.com/org/lib2: v2.1.0
  github.com/dep/lib3: v0.5.0

# Module replacements (optional)
replace:
  github.com/old/lib: github.com/new/lib v1.0.0
  github.com/fork/lib: ../local/path

# Exclusions (optional)
exclude:
  github.com/bad/lib: v1.0.0
  github.com/vuln/lib: v2.1.0

# Retractions (optional)
retract:
  - version: v1.0.1
    rationale: "Critical security vulnerability"
  - version: v1.0.2
    rationale: "Build failure on Windows"
```

### Directive Descriptions

#### `module`
Declares the module's import path. This is the prefix that other modules will use to import packages from this module.

#### `snow`
Specifies the minimum version of Snow required to build this module. Similar to Go's `go` directive.

#### `require`
Lists the direct dependencies of this module with their version constraints. Only direct dependencies should be listed here - indirect dependencies are managed automatically.

Version formats:
- `v1.2.3` - Exact version
- `v1.2.0` - Minimum version (with semantic versioning compatibility)

#### `replace`
Replaces a dependency with another module or local directory. Useful for:
- Using a fork of a dependency
- Local development with unpublished changes
- Working around broken dependencies

#### `exclude`
Excludes specific versions of dependencies. Used to avoid known broken or vulnerable versions.

#### `retract`
Marks versions of the current module as retracted. This is metadata for the module author to signal that certain versions should not be used.

## snow.sum File Specification

The `snow.sum` file contains cryptographic checksums for all dependencies to ensure build reproducibility and security.

### Format

The `snow.sum` file uses the same format as Go's `go.sum`:

```
github.com/user/lib1 v1.2.3 h1:abc123def456ghi789jkl012mno345pqr678stu901vwx=
github.com/user/lib1 v1.2.3/snow.mod h1:def456ghi789jkl012mno345pqr678stu901vwx234yza=
github.com/org/lib2 v2.1.0 h1:ghi789jkl012mno345pqr678stu901vwx234yza567bcd=
github.com/org/lib2 v2.1.0/snow.mod h1:jkl012mno345pqr678stu901vwx234yza567bcd890efg=
```

Each line contains:
- Module path
- Version
- Hash algorithm and base64-encoded checksum

Two types of entries:
- Module source checksum: `<module> <version> <hash>`
- Module file checksum: `<module> <version>/snow.mod <hash>`

### Hash Algorithm

Snow uses SHA-256 for checksums, indicated by the `h1:` prefix (following Go's convention).

## snow mod tidy Command

The `snow mod tidy` command maintains the `snow.mod` and `snow.sum` files automatically.

### Functionality

1. **Source Analysis**: Scans all `.sw` files for import statements
2. **Dependency Resolution**: Uses Minimal Version Selection to resolve dependencies
3. **File Updates**: Updates both `snow.mod` and `snow.sum` with current requirements

### What it does

- Adds missing dependencies that are imported in source code
- Removes unused dependencies from `require`
- Downloads and verifies dependency checksums
- Updates `snow.sum` with new checksums
- Removes unused checksums from `snow.sum`

### Command Options

```bash
snow mod tidy [options]

OPTIONS:
  -v, --verbose    Show detailed output
  -e, --update     Update all dependencies to latest available versions
  --compat=VERSION Maintain compatibility with specified version
```

### Example Usage

```bash
# Basic tidy operation
snow mod tidy

# Verbose output
snow mod tidy -v

# Update all dependencies to latest versions
snow mod tidy -e
```

### Output Example

```bash
$ snow mod tidy
snow: finding github.com/user/lib1 v1.2.3
snow: downloading github.com/user/lib1 v1.2.3
snow: finding github.com/org/lib2 v2.1.0
snow: downloading github.com/org/lib2 v2.1.0
```

## Module Cache

Snow stores downloaded modules in a cache directory similar to Go:

```
$SNOWPATH/pkg/mod/
├── github.com/user/lib1@v1.2.3/
├── github.com/org/lib2@v2.1.0/
└── cache/download/  # Download metadata
```

### Environment Variables

- `SNOWPATH`: Snow workspace directory (defaults to `$HOME/snow`)
- `SNOWPROXY`: Module proxy URL for downloading dependencies
- `SNOWSUMDB`: Checksum database URL for verification

## Integration with Build Commands

The `snow build` and `snow run` commands automatically:
1. Read dependency information from `snow.mod`
2. Verify checksums using `snow.sum`
3. Locate dependencies in the module cache
4. Include dependencies in the build process with static linking

### Build Flags

```bash
snow build --mod=readonly   # Use snow.mod as read-only
snow build --mod=vendor     # Use vendor directory (future feature)
```

## Security Features

1. **Checksum Verification**: All dependencies are verified against `snow.sum`
2. **Reproducible Builds**: Exact versions and checksums ensure consistent builds
3. **Proxy Support**: Optional module proxy for additional security and performance
4. **Retraction Support**: Authors can mark vulnerable versions as retracted

## Migration from Other Systems

When migrating from other package managers:
1. Create initial `snow.mod` with `snow mod init`
2. Add dependencies manually or import from existing configuration
3. Run `snow mod tidy` to generate `snow.sum` and resolve transitive dependencies

This dependency management system provides the same level of reliability and security as Go modules while maintaining Snow's YAML-based configuration philosophy.
