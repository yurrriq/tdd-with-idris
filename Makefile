PKG   ?= typedriven
TEST  ?= test

ifeq "${DEBUG}" "1"
	OPTS := --log 4
endif

# If we're in a Nix shell, call idris directly,
# otherwise use nix-shell --run "idris ..."
# N.B. This requires the -e flag to be set for make.
ifeq "${IN_NIX_SHELL}" "1"
	idris = idris ${OPTS} ${1}
else
	idris = nix-shell --run "idris ${OPTS} ${1}"
endif

.PHONY: build clean check clobber install rebuild test docs docs-clean

build:
	$(call idris,--build ${PKG}.ipkg)

clean:
	$(call idris,--clean ${PKG}.ipkg)

check: clobber
	$(call idris,--checkpkg ${PKG}.ipkg)

clobber: clean docs-clean
	find . -name "*.ibc" -delete

install:
	$(call idris,--install ${PKG}.ipkg)

rebuild: clean lib

test: clean install
	$(call idris,--testpkg ${TEST}.ipkg)

docs: build docs-clean
	$(call idris,--mkdoc ${PKG}.ipkg) \
	&& rm -rf docs >/dev/null \
	&& mv ${PKG}_doc docs

docs-clean:
	rm -rf ${PKG}_doc docs >/dev/null

bin/%: bin/%.idr
	$(call idris,-i src --total -o $@ $<)
