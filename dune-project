(lang dune 3.14)

(cram enable)

(name soteria)

(using dune_site 0.1)

(using mdx 0.4)

(using menhir 3.0)

(generate_opam_files)

(license Apache-2.0)

(authors
 "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>"
 "Opale Sjöstedt <opale.sjostedt23@imperial.ac.uk>")

(maintainers
 "Sacha-Élie Ayoun <s.ayoun17@imperial.ac.uk>"
 "Opale Sjöstedt <opale.sjostedt23@imperial.ac.uk>")

(source
 (github soteria-tools/soteria))

(package
 (name soteria)
 (synopsis "Soteria is a toolkit for writing symbolic bug-finding tools")
 (description "Soteria is a toolkit for writing symbolic bug-finding tools")
 (depends
  ; [versionsync: OCAML_VERSION=5.4.0]
  (ocaml
   (>= 5.4.0))
  (tsort
   (>= 2.1.0))
  eio_main
  simple_smt
  iter
  patricia-tree
  (ppx_expect
   (>= 0.17.0))
  ppx_deriving
  fmt
  printbox-text
  htmlit
  (progress
   (>= 0.5.0))
  (grace
   (>= 0.3.0))
  (odoc :with-doc)
   (mdx :with-test)
   (alcotest :with-test)
   (qcheck-core :with-test)
   (qcheck-alcotest :with-test)
   (dune
   (and
    :build
    (>= 3.14.0)))
  ppxlib
  (hc
   (>= 0.5))
  (yojson
   (< 3.0))
  unionFind
  ppx_deriving_yojson
  ; [versionsync: OCAMLFORMAT_VERSION=0.28.1]
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name soteria-c)
 (synopsis "A C bug-finder based on Soteria")
 (description "A C bug-finder based on Soteria")
 (sites
  (share includes))
 (depends
  ; [versionsync: OCAML_VERSION=5.4.0]
  (ocaml
   (>= 5.4.0))
  (shexp :build)
  eio_main
  soteria
  cerberus-lib
  zarith
  cmdliner
  ppx_deriving
  fmt
  (linol
   (>= 0.10))
  (linol-eio
   (>= 0.10))
  (ppx_subliner
   (>= 0.2.1))
  (odoc :with-doc)
  (alcotest :with-test)
  (ppx_blob
   (and
    :build
    (>= 0.9.0)))
  (yojson
   (< 3.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  ; [versionsync: OCAMLFORMAT_VERSION=0.28.1]
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name soteria-vscode)
 (allow_empty)
 (synopsis "VSCode extension for Soteria-C")
 (description "VSCode extension for Soteria-C")
 (depends
  ; [versionsync: OCAML_VERSION=5.4.0]
  (ocaml
   (>= 5.4.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocaml-lsp-server :with-dev-setup)
  ; [versionsync: OCAMLFORMAT_VERSION=0.28.1]
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))
  js_of_ocaml
  js_of_ocaml-ppx
  ppx_deriving
  promise_jsoo
  vscode
  vscode-interop
  vscode-node))

(package
 (name soteria-rust)
 (synopsis "Rust symbolic execution tool based on Soteria")
 (description "Rust symbolic execution tool based on Soteria")
 (sites
  (share plugins))
 (depends
  ; [versionsync: OCAML_VERSION=5.4.0]
  (ocaml
   (>= 5.4.0))
  (shexp :build)
  soteria
  dune-site
  zarith
  patricia-tree
  ppx_deriving
  cmdliner
  fmt
  unionFind
  name_matcher_parser
  charon
  (ppx_subliner
   (>= 0.2.1))
  (odoc :with-doc)
  (alcotest :with-test)
  (yojson
   (< 3.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  ; [versionsync: OCAMLFORMAT_VERSION=0.28.1]
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))
  (ocaml-lsp-server :with-dev-setup)))
