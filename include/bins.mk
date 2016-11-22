.PHONY: bins

bin/%: bin/%.ipkg src/Main/%/Main.idr
	${MAKE} -C bin -e build PKG=$*

bins: install
	$(foreach bin,${BINS},${MAKE} -B bin/${bin};)
