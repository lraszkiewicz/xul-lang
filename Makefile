SHELL=/usr/bin/env bash

GRAMMAR_FILES=src/Xul.cf build/AbsXul.hs build/LexXul.hs build/ErrM.hs \
	build/ParXul.hs build/PrintXul.hs build/SkelXul.hs build/TestXul.hs

.PHONY: all parser doc run_good

all: parser doc interpreter

build/%.hs: src/Xul.cf
	mkdir -p build; \
	cd build; \
	bnfc -m -haskell ../src/Xul.cf; \
	make

parser: $(GRAMMAR_FILES)

doc/DocXul.html: build/DocXul.txt
	mkdir -p doc; \
	# txt2tags -t html -o doc/DocXul.html build/DocXul.txt
	# txt2tags is not available on students

doc: doc/DocXul.html

interpreter: $(GRAMMAR_FILES) src/Main.hs src/Interpreter.hs src/TypeChecker.hs
	cabal configure; \
	cabal build; \
	cp dist/build/xul-lang/xul-lang interpreter

run_good: interpreter
	@for f in good/*.xul; do \
		echo $$f; \
		# ./interpreter "$$f" +RTS -xc; \
		./interpreter "$$f"; \
	done

# example: all
#   ./build/TestXul example.xul;
