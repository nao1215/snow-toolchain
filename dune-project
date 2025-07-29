(lang dune 3.19)

(name snow)

(generate_opam_files true)

(source
 (github nao1215/snow-toolchain))

(authors "Naohiro CHIKAMATSU <n.chika156@gmail.com>")

(maintainers "Naohiro CHIKAMATSU <n.chika156@gmail.com>")

(license MIT)

(homepage "https://github.com/nao1215/snow-toolchain")

(documentation "https://github.com/nao1215/snow-toolchain#readme")

(package
 (name snow)
 (synopsis
  "SNOW: A modern functional programming language with comprehensive toolchain")
 (description
  "SNOW is an expression-based functional programming language that combines type safety,\n   null safety, and performance with a comprehensive official ecosystem like Go.\n   \n   Key Features:\n   • Rich type system with Option/Result types for guaranteed safety\n   • Expression-based syntax with powerful pattern matching\n   • Automatic currying and first-class higher-order functions  \n   • Go-inspired package system with intuitive visibility rules\n   • LLVM backend for native performance and zero-cost abstractions\n   • Official toolchain: build, run, test, format, package management\n   • Comprehensive standard library and curated package ecosystem\n   \n   SNOW aims to provide the same level of official tooling and ecosystem support\n   that makes Go productive, while offering the expressiveness and safety of\n   modern functional programming languages like Rust and OCaml.")
 (depends
  (ocaml
   (>= 4.14.0))
  (dune
   (>= 3.19))
  (llvm
   (>= 14.0.0))
  (yaml
   (>= 3.2.0))
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (odoc :with-doc))
 (tags ("compiler")))
