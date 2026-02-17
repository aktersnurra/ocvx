(lang dune 3.21)

(name ocvx)

(generate_opam_files true)

(source
 (github aktersnurra/ocvx))

(authors "Gustaf Rydholm <gustaf@gustafrydholm.xyz>")

(maintainers "Gustaf Rydholm <gustaf@gustafrydholm.xyz>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocvx)
 (synopsis "Convex optimization for OCaml with OSQP and SCS solvers")
 (description
  "OCaml bindings to OSQP and SCS quadratic/conic solvers with a high-level DSL for portfolio optimization")
 (depends
  (ocaml (>= 5.0))
  (ctypes (>= 0.20))
  ctypes-foreign
  (ppx_deriving (>= 5.2)))
 (tags
  (optimization "convex-optimization" "quadratic-programming" portfolio osqp scs)))
