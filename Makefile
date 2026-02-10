# [versionsync: OCAML_VERSION=5.4.0]
OCAML_VERSION=5.4.0
# [versionsync: OCAMLFORMAT_VERSION=0.28.1]
OCAMLFORMAT_VERSION=0.28.1

OPAM=opam
OPAMX=$(OPAM) exec --
DUNE=$(OPAMX) dune
WHICHX=$(DUNE) exec -- which

YARN=yarn

DYLIB_LIST_FILE=packaging/soteria-c/macOS_dylibs.txt
PACKAGING_BIN=$(DUNE) exec -- packaging/soteria-c/package.exe
SOTERIA_C_BIN=_build/install/default/bin/soteria-c
VSCODE_BC_JS=vscode/src/soteria_vscode.bc.js

SOTERIA_RUST_DYLIB_LIST_FILE=packaging/soteria-rust/macOS_dylibs.txt
SOTERIA_RUST_PACKAGING_BIN=$(DUNE) exec -- packaging/soteria-rust/package.exe
SOTERIA_RUST_BIN=_build/install/default/bin/soteria-rust

SOTERIA_C_PACKAGE=packages/soteria-c
SOTERIA_RUST_PACKAGE=packages/soteria-rust
VSCODE_DIST=dist

##### Normal ocaml stuff #####
.PHONY: ocaml
ocaml:
	$(DUNE) build

.PHONY: ocaml-format-check
ocaml-format-check:
	$(DUNE) build @fmt
	
.PHONY: check-opam-files
check-opam-files:
	@git diff --name-only HEAD | grep -q '\.opam$$' && echo "Error: .opam files have changed since last commit" && exit 1 || exit 0
	

.PHONY: ocaml-test
ocaml-test:
	$(DUNE) test

.PHONY: doc
doc:
	$(DUNE) build @doc
	chmod u+w _build/default/_doc/_html/odoc.support/odoc.css
	cp doc/odoc-theme/odoc.css _build/default/_doc/_html/odoc.support/odoc.css

##### Packaging soteria-c #####

# From inside the package folder one can run:
# SOTERIA_Z3_PATH=./bin/z3 DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/soteria-c exec-main file.c
.PHONY: package
package: package-soteria-c package-soteria-rust

.PHONY: package-soteria-c
package-soteria-c: ocaml packaging/soteria-c/bin-locations.txt packaging/soteria-c/macOS_dylibs.txt
	$(DUNE) build @soteria-c-dylist-file
	$(PACKAGING_BIN) copy-files $(DYLIB_LIST_FILE) $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-files packaging/soteria-c/bin-locations.txt $(SOTERIA_C_PACKAGE)/bin
	mkdir -p $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-cerb-runtime $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-soteria-c-auto-includes $(SOTERIA_C_PACKAGE)/lib/


packaging/soteria-c/bin-locations.txt:
	$(WHICHX) soteria-c > $@
	$(WHICHX) z3 >> $@

packaging/soteria-c/macOS_dylibs.txt:
	$(PACKAGING_BIN) infer-dylibs $(SOTERIA_C_BIN) > $@

##### Packaging soteria-rust #####

# From inside the package folder one can run:
# SOTERIA_Z3_PATH=./bin/z3 SOTERIA_OBOL_PATH=./bin/obol SOTERIA_CHARON_PATH=./bin/charon \
#   SOTERIA_RUST_PLUGINS=./plugins DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/soteria-rust cargo .
.PHONY: package-soteria-rust
package-soteria-rust: ocaml packaging/soteria-rust/bin-locations.txt packaging/soteria-rust/macOS_dylibs.txt
	$(DUNE) build @soteria-rust-dylist-file
	$(SOTERIA_RUST_PACKAGING_BIN) copy-files $(SOTERIA_RUST_DYLIB_LIST_FILE) $(SOTERIA_RUST_PACKAGE)/lib
	$(SOTERIA_RUST_PACKAGING_BIN) copy-files packaging/soteria-rust/bin-locations.txt $(SOTERIA_RUST_PACKAGE)/bin
	$(SOTERIA_RUST_PACKAGING_BIN) copy-soteria-rust-plugins $(SOTERIA_RUST_PACKAGE)/plugins

packaging/soteria-rust/bin-locations.txt:
	$(WHICHX) soteria-rust > $@
	$(WHICHX) z3 >> $@
	which obol >> $@
	which charon >> $@
	which obol-driver >> $@ 2>/dev/null || true
	which charon-driver >> $@ 2>/dev/null || true

packaging/soteria-rust/macOS_dylibs.txt:
	$(SOTERIA_RUST_PACKAGING_BIN) infer-dylibs $(SOTERIA_RUST_BIN) > $@

##### Switch creation / dependency setup #####

.PHONY: switch
switch:
	$(OPAM) switch create . ocaml-base-compiler.$(OCAML_VERSION) --deps-only --with-test --with-doc -y
	$(OPAM) install ocaml-lsp-server odig ocamlformat.$(OCAMLFORMAT_VERSION) -y

.PHONY: glob-switch
glob-switch:
	$(OPAM) switch create soteria-install ocaml-base-compiler.$(OCAML_VERSION) -y
	$(OPAM) switch soteria-install

.PHONY: install
install:
	$(OPAM) install . -y

.PHONY: ocaml-deps
ocaml-deps:
	$(OPAM) install . --deps-only --with-test --with-doc
	$(OPAM) install ocamlformat.$(OCAMLFORMAT_VERSION)
	$(OPAM) install sherlodoc

##### JavaScript stuff #####

.PHONY: npm-deps
npm-deps:
	$(YARN) install --immutable

.PHONY: vscode-reinstall-dev
vscode-reinstall-dev: vscode-package
	$(YARN) install-ext


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
	$(DUNE) build $(VSCODE_BC_JS)

.PHONY: for-local
for-local: ocaml vscode

.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf packages
	rm -rf $(VSCODE_DIST)
	rm -rf packaging/soteria-c/bin-locations.txt packaging/soteria-c/macOS_dylibs.txt
	rm -rf packaging/soteria-rust/bin-locations.txt packaging/soteria-rust/macOS_dylibs.txt
	rm -f soteria-vscode.vsix

license-check:
	reuse lint

.PHONY: license license-lint
