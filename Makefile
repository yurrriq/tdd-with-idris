SRC       ?= src/Exercises
BIN       ?= bin
OUT       ?= out
LIDR_SRCS := $(notdir $(wildcard $(SRC)/*.lidr))
IBCS      := $(addprefix $(SRC)/, $(LIDR_SRCS:.lidr=.ibc))
BINS      := $(addprefix $(BIN)/, $(LIDR_SRCS:.lidr=))
MDS       := $(addprefix $(OUT)/, $(LIDR_SRCS:.lidr=.md))
PANDOC     = pandoc -f markdown+lhs -t markdown_github
SED_HACK   = sed 's/ sourceCode/idris/'
CLEAN      = rm -f

# all: compile markdown

all: markdown

clobber: clean clean-out
	rm -rf _build/

clean: ; $(CLEAN) $(BINS) $(IBCS)

clean-out: ; $(CLEAN) $(MDS)

# compile: $(BINS)

markdown: $(MDS)

# grip: markdown; @grip $(MDS)

$(OUT)/%.md: $(SRC)/%.lidr; $(PANDOC) $< | $(SED_HACK) > $@

# $(BIN)/%: $(SRC)/%Main.lidr; idris -i src --check -o $@ $<

build: ; idris --build typedriven.ipkg

test: ; nix-shell --command 'idris --testpkg test.ipkg'

doc: ; idris --mkdoc typedriven.ipkg
