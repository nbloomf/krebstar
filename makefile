SOURCES := \
  src/control/Kreb/Control/ReplT.lhs \
  src/struct/Kreb/Struct/FingerTree.lhs \
  src/struct/Kreb/Struct/OnePointedList.lhs \
  src/struct/Kreb/Struct/Seq.lhs \
  src/struct/Kreb/Struct/RunLengthEncoded.lhs \
  src/struct/Kreb/Struct/TwoPointedList.lhs \
  src/text/Kreb/Text/Buffer.lhs

EXTRADOC := \
  aux/section/Rationale.md



all: test docs install

test: generative golden



golden: FORCE
	@echo 'Running golden tests...'
	@shelltest --hide-successes --color golden/

generative: FORCE
	@echo 'Running generative tests...'
	@stack test krebstar:kreb-test

install: FORCE
	@echo 'Installing...'
	@stack install



docs: docs/index.html $(SOURCES) $(EXTRADOC)
	@echo 'Docs Generated'

docs/index.html: FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --data-dir=aux --template=tufte.html5 \
	  --css style.css \
	  --output $@ \
	  aux/index.md

$(EXTRADOC): FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --filter pandoc-sidenote \
	  --data-dir=aux --template=tufte.html5 \
	  --css ../style.css \
	  --output docs/html/$(patsubst %.md,%.html,$(notdir $@)) \
	  $@

$(SOURCES): FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --filter pandoc-sidenote \
	  --data-dir=aux --template=tufte.html5 \
	  --css ../style.css \
	  --output docs/html/$(patsubst %.lhs,%.html,$(notdir $@)) \
	  $@

FORCE:

.PHONY: all test docs install golden generative FORCE
