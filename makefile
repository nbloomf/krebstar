SOURCES := \
  src/control/Kreb/Control/ReplT.lhs \
  src/struct/Kreb/Struct/FingerTree.lhs \
  src/struct/Kreb/Struct/OnePointedList.lhs \
  src/struct/Kreb/Struct/Sequence.lhs \
  src/struct/Kreb/Struct/RunLengthEncoded.lhs \
  src/struct/Kreb/Struct/TwoPointedList.lhs \
  src/struct/Kreb/Struct/RoseTree.lhs \
  src/struct/Kreb/Struct/PointedRoseTree.lhs \
  src/text/Kreb/Text/Buffer.lhs \
  src/text/Kreb/Text/SizedBuffer.lhs \
  src/text/Kreb/Text/TextBox.lhs

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
	  --data-dir=aux --template=doc.html5 \
	  --css style.css \
	  --output $@ \
	  aux/index.md

$(EXTRADOC): FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --filter pandoc-sidenote \
	  --data-dir=aux --template=doc.html5 \
	  --css ../style.css \
	  --output docs/html/$(patsubst %.md,%.html,$(notdir $@)) \
	  $@

$(SOURCES): FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --filter pandoc-sidenote \
	  --data-dir=aux --template=doc.html5 \
	  --css ../style.css \
	  --output docs/html/$(patsubst %.lhs,%.html,$(notdir $@)) \
	  $@

FORCE:

.PHONY: all test docs install golden generative FORCE
