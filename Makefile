# OCaml stuff first	
.PHONY: ocaml
ocaml:
	dune build @all
	
.PHONY: ocaml-test
ocaml-test:
	dune test

.PHONY: switch
switch:
	opam switch create . --deps-only --with-test --with-doc

.PHONY: npm-deps
npm-deps:
	yarn install --immutable
	
.PHONY: vscode-package
vscode-package: vscode
	yarn package
	
.PHONY: vscode
vscode: js-bundle


.PHONY: js-bundle
js-bundle: vscode-ocaml
	yarn compile
	

.PHONY: vscode-ocaml
vscode-ocaml:
	dune build extension/bfa_vscode.bc.js
