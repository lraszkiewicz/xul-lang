SHELL=/usr/bin/env bash

GRAMMAR_FILES=src/Xul.cf build_bnfc/AbsXul.hs build_bnfc/LexXul.hs \
	build_bnfc/ErrM.hs build_bnfc/ParXul.hs build_bnfc/PrintXul.hs \
	build_bnfc/SkelXul.hs build_bnfc/TestXul.hs

.PHONY: all parser doc run_good run_bad clean

all: parser interpreter

build_bnfc/%.hs: src/Xul.cf
	mkdir -p build_bnfc; \
	cd build_bnfc; \
	bnfc -m -haskell ../src/Xul.cf; \
	make

parser: $(GRAMMAR_FILES)

doc/DocXul.html: build_bnfc/DocXul.txt
	mkdir -p doc; \
	txt2tags -t html -o doc/DocXul.html build_bnfc/DocXul.txt

doc: doc/DocXul.html

interpreter: $(GRAMMAR_FILES) src/Main.hs src/Interpreter.hs src/TypeChecker.hs
	ghc src/Main.hs -O2 -isrc/ -ibuild_bnfc/ -o interpreter

run_good: interpreter
	@for f in good/*.xul; do \
		echo "$$f"; \
		# ./interpreter "$$f" +RTS -xc; \
		./interpreter "$$f"; \
	done

run_bad: interpreter
	@for f in bad/*.xul; do \
		echo "$$f"; \
		# ./interpreter "$$f" +RTS -xc; \
		./interpreter "$$f"; \
	done

clean:
	rm -rf build_bnfc/ doc/ interpreter src/*.o src/*.hi
