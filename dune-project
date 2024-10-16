(lang dune 3.14)

(cram enable)

(name bfa)

(generate_opam_files)

(license BSD-3-Clause)

(authors "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>")

(maintainers "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>")

(source
 (github bugs-for-all/bfa))

(package
 (name bfa)
 (synopsis "Bugs for All!")
 (description "Bugs for All!")
 (depends
  (ocaml
   (>= 5.2.0))
  eio_main
  simple_smt
  (ppx_expect
   (>= 0.17.0))
  ppx_deriving
  fmt
  logs
  (odoc :with-doc)
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocamlformat
   (and
    :dev
    (= 0.26.2)))
  (ocaml-lsp-server :dev)))

(package
 (name bfa-c)
 (synopsis "Bugs for All, for C.")
 (description "Bugs for All, for C.")
 (depends
  (ocaml
   (>= 5.2.0))
  eio_main
  bfa
  ppx_inline_test
  cerberus-lib
  zarith
  ppx_deriving
  hashcons
  cmdliner
  fmt
  logs
  (linol
   (>= 0.6))
  (linol-eio
   (>= 0.6))
  (odoc :with-doc)
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocamlformat
   (and
    :dev
    (= 0.26.2)))
  (ocaml-lsp-server :dev)))

(package
 (name bfa-vscode)
 (allow_empty)
 (synopsis "VSCode extension for BFA")
 (description "VSCode extension for BFA")
 (depends
  (ocaml
   (>= 5.2.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocaml-lsp-server :dev)
  js_of_ocaml
  js_of_ocaml-ppx
  ppx_deriving
  promise_jsoo
  vscode))
