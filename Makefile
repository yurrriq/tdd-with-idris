SRC       ?= src/Exercises
BIN       ?= bin
OUT       ?= out
LIDR_SRCS := $(notdir $(wildcard $(SRC)/*.lidr))
BIN_SRCS  := $(notdir $(wildcard bin/*.idr))
BINS      := $(addprefix $(BIN)/, $(BIN_SRCS:.idr=))
MDS       := $(addprefix $(OUT)/, $(LIDR_SRCS:.lidr=.md))
PANDOC     = pandoc -f markdown+lhs -t markdown_github
SED_HACK   = sed 's/ sourceCode/idris/'
CLEAN      = rm -f

all: check

clobber: clean # clean-out
	rm -rf _build/

clean:
	idris --clean typedriven.ipkg
	find . -name '*.ibc' -delete

clean-out: ; $(CLEAN) $(MDS)

compile: $(BINS)

markdown: $(MDS)

# grip: markdown; @grip $(MDS)

$(OUT)/%.md: $(SRC)/%.lidr; $(PANDOC) $< | $(SED_HACK) > $@

$(BIN)/%: bin/%.idr; idris -i src --total -o $@ $<

build: ; idris --build typedriven.ipkg

check: ; idris --checkpkg typedriven.ipkg

test: ; idris --testpkg test.ipkg

doc: ; idris --mkdoc typedriven.ipkg
