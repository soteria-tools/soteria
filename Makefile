OPAM=opam
OPAMX=$(OPAM) exec --
DUNE=$(OPAMX) dune
WHICHX=$(DUNE) exec -- which 

YARN=yarn

DYLIB_LIST_FILE=packaging/macOS_dylibs.txt
PACKAGING_BIN=$(DUNE) exec -- packaging/package.exe
BFA_C_BIN=_build/install/default/bin/bfa-c

PACKAGE_DIST=package

##### Normal ocaml stuff #####
.PHONY: ocaml
ocaml:
	 $(DUNE) build

.PHONY: ocaml-test
ocaml-test:
	$(DUNE) test
	
	
##### Packaging bfa-c #####

# From inside the package folder one can run:
# BFA_Z3_PATH=./bin/z3 DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/bfa-c exec-main file.c
.PHONY: package
package: ocaml packaging/bin-locations.txt packaging/macOS_dylibs.txt
	$(DUNE) build @dylist-file
	$(PACKAGING_BIN) copy-files $(DYLIB_LIST_FILE) $(PACKAGE_DIST)/lib
	$(PACKAGING_BIN) copy-files packaging/bin-locations.txt $(PACKAGE_DIST)/bin
	rm -f package.zip
	zip package.zip -r $(PACKAGE_DIST)
	

packaging/bin-locations.txt:
	$(WHICHX) bfa-c > $@
	$(WHICHX) z3 >> $@

packaging/macOS_dylibs.txt:
	$(PACKAGING_BIN) infer-dylibs $(BFA_C_BIN) > $@

##### Switch creation / dependency setup #####

.PHONY: switch
switch:
	$(OPAM) switch create . --deps-only --with-test --with-doc
	
.PHONY: ocaml-deps
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
	
.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf $(PACKAGE_DIST)
	rm -rf packaging/bin-locations.txt packaging/macOS_dylibs.txt