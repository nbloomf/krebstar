help: FORCE
	@echo "clean        Delete all temporary and intermediate files"
	@echo "rebuild      Recompile all libraries from scratch"
	@echo "generative   Run all generative tests (slow!)"
	@echo "docs         Generate documentation"
	@echo "all          Do everything (slow!)"



#-----#
# ALL #
#-----#

all: test docs install



#-------#
# CLEAN #
#-------#

clean: FORCE
	@(rm -rf docs/html)
	@(cd lib/kreb-arith && rm -rf .stack-work/)
	@(cd lib/kreb-check && rm -rf .stack-work/)
	@(cd lib/kreb-control && rm -rf .stack-work/)
	@(cd lib/kreb-category && rm -rf .stack-work/)
	@(cd lib/kreb-editor && rm -rf .stack-work/)
	@(cd lib/kreb-editor-core && rm -rf .stack-work/)
	@(cd lib/kreb-editor-tui && rm -rf .stack-work/)
	@(cd lib/kreb-effect && rm -rf .stack-work/)
	@(cd lib/kreb-format && rm -rf .stack-work/)
	@(cd lib/kreb-lang && rm -rf .stack-work/)
	@(cd lib/kreb-reflect && rm -rf .stack-work/)
	@(cd lib/kreb-struct && rm -rf .stack-work/)
	@(cd lib/kreb-text && rm -rf .stack-work/)
	@(cd lib/kreb-unit && rm -rf .stack-work/)



#-------#
# BUILD #
#-------#

rebuild: clean \
  kreb-arith \
  kreb-check \
  kreb-control \
  kreb-category \
  kreb-editor-core \
  kreb-editor-tui \
  kreb-effect \
  kreb-format \
  kreb-lang \
  kreb-reflect \
  kreb-struct \
  kreb-text \
  kreb-unit \

kreb-control: FORCE
	cd lib/kreb-control && stack build

kreb-format: kreb-control
	cd lib/kreb-format && stack build

kreb-check: kreb-control kreb-format
	cd lib/kreb-check && stack build

kreb-unit: kreb-control kreb-format
	cd lib/kreb-unit && stack build

kreb-reflect: kreb-check
	cd lib/kreb-reflect && stack build

kreb-arith: kreb-check kreb-reflect
	cd lib/kreb-arith && stack build

kreb-struct: kreb-check kreb-reflect kreb-control kreb-arith
	cd lib/kreb-struct && stack build

kreb-effect: kreb-check kreb-control kreb-struct
	cd lib/kreb-effect && stack build

kreb-text: kreb-check kreb-reflect kreb-control kreb-struct kreb-arith kreb-effect
	cd lib/kreb-text && stack build

kreb-lang: kreb-check
	cd lib/kreb-lang && stack build

kreb-editor-core: kreb-check kreb-reflect kreb-arith kreb-effect kreb-control kreb-struct kreb-text kreb-lang
	cd lib/kreb-editor-core && stack build

kreb-editor-tui: kreb-check kreb-reflect kreb-arith kreb-effect kreb-control kreb-struct kreb-text kreb-lang kreb-editor-core
	cd lib/kreb-editor-tui && stack build



#------#
# TEST #
#------#

test: golden generative

golden: FORCE
	@echo 'Running golden tests...'
	@shelltest --hide-successes --color golden/

generative: \
  kreb-format-test \
  kreb-check-test \
  kreb-control-test \
  kreb-reflect-test \
  kreb-arith-test \
  kreb-struct-test \
  kreb-effect-test \
  kreb-text-test \
  kreb-lang-test \
  kreb-editor-core-test \
  kreb-editor-tui-test

kreb-format-test: kreb-format
	cd lib/kreb-format && stack test

kreb-check-test: kreb-check
	cd lib/kreb-check && stack test

kreb-control-test: kreb-control
	cd lib/kreb-control && stack test

kreb-reflect-test: kreb-reflect
	cd lib/kreb-reflect && stack test

kreb-arith-test: kreb-arith
	cd lib/kreb-arith && stack test

kreb-struct-test: kreb-struct
	cd lib/kreb-struct && stack test

kreb-effect-test: kreb-effect
	cd lib/kreb-effect && stack test

kreb-text-test: kreb-text
	cd lib/kreb-text && stack test

kreb-lang-test: kreb-lang
	cd lib/kreb-lang && stack test

kreb-editor-core-test: kreb-editor-core
	cd lib/kreb-editor-core && stack test

kreb-editor-tui-test: kreb-editor-tui
	cd lib/kreb-editor-tui && stack test



#------#
# MISC #
#------#

FORCE:

.PHONY: all test docs install golden generative FORCE
