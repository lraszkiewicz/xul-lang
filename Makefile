SHELL=/usr/bin/env bash

all:
	mkdir -p build; \
	cd build; \
	bnfc -m -haskell ../Xul.cf; \
	make; \
	txt2tags -t html -o DocXul.html DocXul.txt;

example: all
	./build/TestXul example.xul;
