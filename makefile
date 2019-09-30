all: test docs install

test: golden generative

golden:
	@echo 'Running golden tests...'
	@shelltest --hide-successes --color golden/

generative:
	@echo 'Running generative tests...'
	@stack test krebstar:kreb-test

install:
	@echo 'Installing...'
	@stack install



docfiles = \
  docs/src/Ned/Data/FingerTree.html \
  docs/src/Ned/Data/FingerTreeZip.html \
  docs/src/Ned/Data/Seq.html \
  docs/src/Ned/Data/RunLengthEncoding.html \
  docs/src/Ned/Data/ReflectNat.html \
  docs/src/Ned/Data/ScreenOffset.html \
  docs/src/Ned/Data/MeasureText.html \
  docs/src/Ned/Data/Buffer.html \
  docs/src/Ned/Data/TextBox.html \
  docs/test/Ned/Data/FingerTree/Test.html \
  docs/test/Ned/Data/FingerTreeZip/Test.html \
  docs/test/Ned/Data/Seq/Test.html \
  docs/test/Ned/Data/RunLengthEncoding/Test.html \
  docs/test/Ned/Data/ReflectNat/Test.html \
  docs/test/Ned/Data/ScreenOffset/Test.html \
  docs/test/Ned/Data/MeasureText/Test.html \
  docs/test/Ned/Data/Buffer/Test.html \
  docs/test/Ned/Data/TextBox/Test.html

docs: $(docfiles) docs/index.html
	@echo 'Docs Generated'

docs/index.html: aux/index.md
	@pandoc --mathjax -s -H aux/style.txt -o $@ $<	

docs/%.html: %.lhs
	@echo 'Generating $< ==> $@'
	@pandoc --mathjax -s -H aux/style.txt -o $@ $<

.PHONY: all test docs install golden generative
