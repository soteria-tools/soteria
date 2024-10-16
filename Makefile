OPAM=opam
OPAMX=$(OPAM) exec --
DUNE=$(OPAMX) dune

YARN=yarn

DYLIB_LIST_FILE=packaging/macOS_dylibs.txt
PACKAGING_BIN=$(DUNE) exec -- packaging/package.exe

PACKAGE_DIST=package

##### Normal ocaml stuff #####
.PHONY: ocaml
ocaml:
	 $(DUNE) build

.PHONY: ocaml-test
ocaml-test:
	$(DUNE) test
	
	
##### Packaging bfa-c #####
.PHONY: package
package:
	rm -rf $(PACKAGE_DIST)
	$(DUNE) build @dylist-file
	$(PACKAGING_BIN) copy-libs $(DYLIB_LIST_FILE) $(PACKAGE_DIST)/lib
# TODO: the rm -rf should not be there, the packaging script should be removing existing dylibs to avoid errors
# Next thing to do: copy the bfa-c binary, copy a version of z3? or just leave it to do on the extension's side?
# Ask manon to try with the following command: BFA_Z3_PATH=bin/z3 DYLD_LIBRARY_PATH=lib:$DYLD_LIBRARY_PATH bin/bfa_c.exe exec-main test/simple.t/sym.c
	


##### Switch creation / dependency setup #####

.PHONY: switch
switch:
	$(OPAM) switch create . --deps-only --with-test --with-doc
	
ocaml-deps:
	$(OPAM) install . --deps-only --with-test --with-doc

##### JavaScript stuff #####

.PHONY: npm-deps
npm-deps:
	$(YARN) install --immutable
	
.PHONY: vscode-package
vscode-package: vscode
	$(YARN) package
	
.PHONY: vscode
vscode: js-bundle


.PHONY: js-bundle
js-bundle: vscode-ocaml
	$(YARN) compile
	

.PHONY: vscode-ocaml
vscode-ocaml:
	$(DUNE) build extension/bfa_vscode.bc.js
