ifeq "${DEBUG}" "1"
	OPTS := --log 4
else ifneq "${LOG}" ""
  OPTS := --log ${LOG}
endif

# If we're in a Nix shell, call idris directly,
# otherwise use nix-shell --run "idris ..."
# N.B. This requires the -e flag to be set for make.
ifeq "${IN_NIX_SHELL}" "1"
	idris = idris ${OPTS} ${1}
else
	idris = nix-shell --run "idris ${OPTS} ${1}"
endif

.PHONY: build clean check install rebuild

build:
	$(call idris,--build ${PKG}.ipkg)

clean:
	$(call idris,--clean ${PKG}.ipkg)

check: clean
	$(call idris,--checkpkg ${PKG}.ipkg)

install:
	$(call idris,--install ${PKG}.ipkg)

rebuild: clean build
