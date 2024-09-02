(lang dune 3.14)

(name bfa)

(generate_opam_files)

(license BSD-3-Clause)

(authors "Sacha-\195\137lie Ayoun <s.ayoun17@imperial.ac.uk>")

(maintainers "Sacha-\195\137lie Ayoun <s.ayoun17@imperial.ac.uk>")

(source
 (github bugs-for-all/bfa))

(package
 (name bfa)
 (synopsis "Bugs for All!")
 (description "Bugs for All!")
 (depends
  (ocaml
   (>= 5.2.0))
  (ocamlformat
   (and
    :dev
    (>= 1.3.0)))
  (ocaml-lsp-server :dev)
  (odoc :with-doc)
  (dune
   (and
    :build
    (>= 3.14.0)))))
