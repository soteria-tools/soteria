# [versionsync: OCAML_VERSION=5.4.1]
OCAML_VERSION=5.4.1
# [versionsync: OCAMLFORMAT_VERSION=0.28.1]
OCAMLFORMAT_VERSION=0.28.1
# [versionsync: DUNE_VERSION=3.23.1]
DUNE_VERSION=3.23.1

OPAM=opam
OPAMX=$(OPAM) exec --
DUNE=$(OPAMX) dune
WHICHX=$(DUNE) exec -- which

PACKAGING_BIN=$(DUNE) exec -- packaging/soteria-c/package.exe
SOTERIA_C_BIN=_build/install/default/bin/soteria-c

SOTERIA_RUST_PACKAGING_BIN=$(DUNE) exec -- packaging/soteria-rust/package.exe
SOTERIA_RUST_BIN=_build/install/default/bin/soteria-rust

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  DYLIB_LIST_FILE=packaging/soteria-c/macOS_dylibs.txt
  SOTERIA_RUST_DYLIB_LIST_FILE=packaging/soteria-rust/macOS_dylibs.txt
else
  DYLIB_LIST_FILE=packaging/soteria-c/linux_dylibs.txt
  SOTERIA_RUST_DYLIB_LIST_FILE=packaging/soteria-rust/linux_dylibs.txt
endif

SOTERIA_C_PACKAGE=packages/soteria-c
SOTERIA_RUST_PACKAGE=packages/soteria-rust

COLOR_BLUE=\033[1;34m
COLOR_GREEN=\033[1;32m
COLOR_RESET=\033[0m

##### Normal ocaml stuff #####
.PHONY: ocaml
ocaml:
	$(DUNE) build

.PHONY: ocaml-format-check
ocaml-format-check:
	$(DUNE) build @fmt

.PHONY: mld-format
mld-format:
	find . -name "*.mld" -not -path "./_opam/*" -not -path "./_build/*" -exec $(OPAMX) ocamlformat --doc --inplace {} \;

.PHONY: ocaml-test
ocaml-test:
	$(DUNE) test

.PHONY: ocaml-slow-tests
ocaml-slow-tests:
	$(DUNE) build @slow-tests

.PHONY: ocaml-very-slow-tests
ocaml-very-slow-tests:
	$(DUNE) build @very-slow-tests

.PHONY: doc
doc:
	$(DUNE) build @doc
	chmod u+w _build/default/_doc/_html/odoc.support/odoc.css
	cp doc/odoc-theme/odoc.css _build/default/_doc/_html/odoc.support/odoc.css

.PHONY: doc-json
doc-json:
	$(DUNE) build @doc-json --only-packages soteria

##### Packaging soteria-c #####

# From inside the package folder one can run:
# SOTERIA_Z3_PATH=./bin/z3 DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/soteria-c exec-main file.c
.PHONY: package
package: package-soteria-c package-soteria-rust

.PHONY: package-soteria-c
package-soteria-c: ocaml packaging/soteria-c/bin-locations.txt $(DYLIB_LIST_FILE)
	$(DUNE) build @soteria-c-dylist-file
	$(PACKAGING_BIN) copy-files $(DYLIB_LIST_FILE) $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-files packaging/soteria-c/bin-locations.txt $(SOTERIA_C_PACKAGE)/bin
	mkdir -p $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-cerb-runtime $(SOTERIA_C_PACKAGE)/lib
	$(PACKAGING_BIN) copy-soteria-c-auto-includes $(SOTERIA_C_PACKAGE)/lib/


packaging/soteria-c/bin-locations.txt:
	$(WHICHX) soteria-c > $@
	$(WHICHX) z3 >> $@ 2>/dev/null || true

packaging/soteria-c/macOS_dylibs.txt:
	$(PACKAGING_BIN) infer-dylibs $(SOTERIA_C_BIN) > $@

packaging/soteria-c/linux_dylibs.txt:
	$(PACKAGING_BIN) infer-dylibs $(SOTERIA_C_BIN) > $@

##### Packaging soteria-rust #####

# From inside the package folder one can run:
# SOTERIA_Z3_PATH=./bin/z3 SOTERIA_OBOL_PATH=./bin/obol SOTERIA_CHARON_PATH=./bin/charon \
#   SOTERIA_RUST_PLUGINS=./plugins DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/soteria-rust exec .
.PHONY: package-soteria-rust
package-soteria-rust: ocaml packaging/soteria-rust/bin-locations.txt $(SOTERIA_RUST_DYLIB_LIST_FILE)
	$(DUNE) build @soteria-rust-dylist-file
	$(SOTERIA_RUST_PACKAGING_BIN) copy-files $(SOTERIA_RUST_DYLIB_LIST_FILE) $(SOTERIA_RUST_PACKAGE)/lib
	$(SOTERIA_RUST_PACKAGING_BIN) copy-files packaging/soteria-rust/bin-locations.txt $(SOTERIA_RUST_PACKAGE)/bin
	$(SOTERIA_RUST_PACKAGING_BIN) copy-soteria-rust-plugins $(SOTERIA_RUST_PACKAGE)/plugins

packaging/soteria-rust/bin-locations.txt:
	$(WHICHX) soteria-rust > $@
	$(WHICHX) z3 >> $@ 2>/dev/null || true
	which obol >> $@ 2>/dev/null || true
	which charon >> $@ 2>/dev/null || true
	which obol-driver >> $@ 2>/dev/null || true
	which charon-driver >> $@ 2>/dev/null || true

packaging/soteria-rust/macOS_dylibs.txt:
	$(SOTERIA_RUST_PACKAGING_BIN) infer-dylibs $(SOTERIA_RUST_BIN) > $@

packaging/soteria-rust/linux_dylibs.txt:
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

.PHONY: soteria-core-deps
soteria-core-deps:
	$(OPAM) install ./soteria.opam --deps-only --with-test

.PHONY: ocaml-deps
ocaml-deps:
	$(OPAM) install dune.$(DUNE_VERSION) ocamlformat.$(OCAMLFORMAT_VERSION) -y
	$(OPAM) install . --deps-only --with-test --with-doc -y
	$(OPAM) install sherlodoc -y

# Clears all *.ullbc, Cargo.lock and target/ subflders in soteria-rust/test/cram/
# We make sure that every file or folder deleted is displayed in the terminal.
.PHONY: clean-rust-tests
clean-rust-tests:
	@echo "$(COLOR_BLUE)==> Cleaning Rust test artifacts in soteria-rust/test/cram/$(COLOR_RESET)"
	@find soteria-rust/test/cram/ -type f -name '*.ullbc' -print -delete | sed 's/^/  🗑️ /'
	@find soteria-rust/test/cram/ -type f -name '*.crate' -print -delete | sed 's/^/  🗑️ /'
	@find soteria-rust/test/cram/ -type f -name 'Cargo.lock' -print -delete | sed 's/^/  🗑️ /'
	@find soteria-rust/test/cram/ -type d -name 'target' -print -exec rm -rf {} + | sed 's/^/  🗑️ /'
	@echo "$(COLOR_GREEN)==> Done$(COLOR_RESET)"



.PHONY: clean
clean: clean-rust-tests
	$(DUNE) clean
	rm -rf packages
	rm -rf packaging/soteria-c/bin-locations.txt packaging/soteria-c/macOS_dylibs.txt packaging/soteria-c/linux_dylibs.txt
	rm -rf packaging/soteria-rust/bin-locations.txt packaging/soteria-rust/macOS_dylibs.txt packaging/soteria-rust/linux_dylibs.txt

license-check:
	reuse lint

.PHONY: license license-lint
