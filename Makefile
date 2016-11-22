PKG   := typedriven
TEST  := test
IPKGS := $(wildcard bin/*.ipkg)
BINS  := $(notdir $(IPKGS:.ipkg=))

include include/core.mk
include include/docs.mk
include include/bins.mk

.PHONY: clobber test

clobber: clean
	find . -name "*.ibc" -delete
	$(foreach bin,${BINS},${MAKE} -C bin -e clean PKG=${bin};)

test: clean install
	$(call idris,--testpkg ${TEST}.ipkg)
