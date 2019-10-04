SOURCES := \
  src/control/Kreb/Control/ReplT.lhs

STYLES := \
  tufte/tufte.css \
  tufte/pandoc.css \
  tufte/pandoc-solarized.css

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



docs: docs/index.html $(SOURCES)
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

$(SOURCES): FORCE
	@echo $@
	@pandoc \
	  --from markdown+literate_haskell --to html \
	  --mathjax --section-divs \
	  --data-dir=aux --template=tufte.html5 \
	  --css ../style.css \
	  --output docs/html/$(patsubst %.lhs,%.html,$(notdir $@)) \
	  $@

FORCE:

.PHONY: all test docs install golden generative FORCE
