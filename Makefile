SHELL=/usr/bin/env bash

.PHONY: all parser doc

all: parser doc interpreter

build/%.hs: src/Xul.cf
	mkdir -p build; \
	cd build; \
	bnfc -m -haskell ../src/Xul.cf; \
	make

parser: src/Xul.cf build/AbsXul.hs build/LexXul.hs build/ErrM.hs \
	build/ParXul.hs build/PrintXul.hs build/SkelXul.hs build/TestXul.hs

doc/DocXul.html: build/DocXul.txt
	mkdir -p doc; \
	txt2tags -t html -o doc/DocXul.html build/DocXul.txt

doc: doc/DocXul.html

interpreter: parser src/Main.hs src/Interpreter.hs
	cabal build; \
	cp dist/build/xul-lang/xul-lang interpreter

# example: all
#   ./build/TestXul example.xul;
