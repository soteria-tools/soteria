(lang dune 3.14)

(cram enable)

(name soteria)

(using dune_site 0.1)

(using mdx 0.4)

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

(pin
 (url
  "git+https://github.com/NatKarmios/simple-smt-ocaml#9db7ef0bb61a76010de255af3cbe4066dd8dd622")
 (package
  (name simple_smt)))

(pin
 (url "git+https://github.com/kmemarian/cerberus#frontend-for-system-libc")
 (package
  (name cerberus-lib)))

(pin
 (url
  "git+https://github.com/giltho/charon#852f899fd8fe3d7501a5de352fdfd3fe2f6b0ef0")
 (package
  (name name_matcher_parser)))

(pin
 (url
  "git+https://github.com/giltho/charon#852f899fd8fe3d7501a5de352fdfd3fe2f6b0ef0")
 (package
  (name charon)))

(pin
 (url
  "git+https://github.com/ocamllabs/vscode-ocaml-platform#205efaf51a709a9ef23a1ba62f32db571a7f6a8c")
 (package
  (name vscode)))

(pin
 (url
  "git+https://github.com/ocamllabs/vscode-ocaml-platform#205efaf51a709a9ef23a1ba62f32db571a7f6a8c")
 (package
  (name vscode-node)))

(pin
 (url
  "git+https://github.com/ocamllabs/vscode-ocaml-platform#205efaf51a709a9ef23a1ba62f32db571a7f6a8c")
 (package
  (name vscode-interop)))

(package
 (name soteria)
 (synopsis "Soteria is a toolkit for writing symbolic bug-finding tools")
 (description "Soteria is a toolkit for writing symbolic bug-finding tools")
 (depends
  (ocaml
   (= 5.3.0))
  (tsort
   (>= 2.1.0))
  eio_main
  simple_smt
  iter
  (ppx_expect
   (>= 0.17.0))
  ppx_deriving
  fmt
  printbox-text
  htmlit
  (progress
   (>= 0.5.0))
  grace
  (odoc :with-doc)
  (mdx :with-test)
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
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name soteria-c)
 (synopsis "A C bug-finder based on Soteria")
 (description "A C bug-finder based on Soteria")
 (sites
  (share includes))
 (depends
  (ocaml
   (>= 5.3.0))
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
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name soteria-vscode)
 (allow_empty)
 (synopsis "VSCode extension for Soteria-C")
 (description "VSCode extension for Soteria-C")
 (depends
  (ocaml
   (>= 5.3.0))
  (dune
   (and
    :build
    (>= 3.14.0)))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
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
  (ocaml
   (>= 5.3.0))
  (shexp :build)
  soteria
  dune-site
  zarith
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
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (ocaml-lsp-server :with-dev-setup)))
