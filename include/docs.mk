.PHONY: docs docs-clean

docs: build docs-clean
	$(call idris,--mkdoc ${PKG}.ipkg) \
	&& rm -rf docs >/dev/null \
	&& mv ${PKG}_doc docs

docs-clean:
	rm -rf ${PKG}_doc docs >/dev/null
