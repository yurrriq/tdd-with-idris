SRC       ?= src
BIN       ?= bin
OUT       ?= out
LIDR_SRCS := $(notdir $(wildcard $(SRC)/*.lidr))
IBCS      := $(addprefix $(SRC)/, $(LIDR_SRCS:.lidr=.ibc))
BINS      := $(addprefix $(BIN)/, $(LIDR_SRCS:.lidr=))
MDS       := $(addprefix $(OUT)/, $(LIDR_SRCS:.lidr=.md))
PANDOC     = pandoc -f markdown+lhs -t markdown_github
SED_HACK   = sed 's/ sourceCode/idris/'
CLEAN      = rm -f

all: compile markdown

clean-all: clean clean-out

clean: ; $(CLEAN) $(BINS) $(IBCS)

clean-out: ; $(CLEAN) $(MDS)

compile: $(BINS)

markdown: $(MDS)

grip: markdown; @grip $(MDS)

out/%.md: src/%.lidr; $(PANDOC) $< | $(SED_HACK) > $@

bin/%: src/%.lidr; idris $< -o $@
