(lang dune 3.20)

(name ppx_memo)

(generate_opam_files true)

(source (github ericlovesmath/ppx_memo))

(authors "Eric Lee <me@ericchanlee.com>")
(maintainers "Eric Lee <me@ericchanlee.com>")

(license MIT)

(documentation https://github.com/ericlovesmath/ppx_memo)

(package
 (name ppx_memo)
 (synopsis "PPX rewriter to memoize functions in OCaml")
 (description "PPX rewriter to memoize functions in OCaml")
 (depends ocaml)
 (tags
  ("ppx" "memoization" "memo" "cache")))
