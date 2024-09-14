(lang dune 3.14)

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
  simple-smt-ocaml
  (ppx_expect
   (>= 0.17.0))
  (core
   (>= 0.17.1))
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
    (>= 1.3.0)))
  (ocaml-lsp-server :dev)))
