SHELL=/usr/bin/env bash

STUDENTS_BNFC=/home/students/inf/PUBLIC/MRJP/bin/bnfc
ifneq ("$(wildcard $(STUDENTS_BNFC))","")
	BNFC=$(STUDENTS_BNFC)
else
	BNFC=bnfc
endif

GRAMMAR_FILES=src/Xul.cf build_bnfc/AbsXul.hs build_bnfc/LexXul.hs \
	build_bnfc/ErrM.hs build_bnfc/ParXul.hs build_bnfc/PrintXul.hs \
	build_bnfc/SkelXul.hs build_bnfc/TestXul.hs

.PHONY: all parser doc run_good run_bad clean zip

all: parser interpreter

build_bnfc/%.hs: src/Xul.cf
	mkdir -p build_bnfc; \
	cd build_bnfc; \
	$(BNFC) -m -haskell ../src/Xul.cf; \
	make

parser: $(GRAMMAR_FILES)

parser_for_stack: src/Xul.cf
	mkdir -p build_bnfc_stack; \
	cd build_bnfc_stack; \
	$(BNFC) -m -haskell ../src/Xul.cf; \
	make; \
	rm *.hi *.o *.txt *.x *.y Makefile TestXul.hs TestXul

doc/DocXul.html: build_bnfc/DocXul.txt
	mkdir -p doc; \
	txt2tags -t html -o doc/DocXul.html build_bnfc/DocXul.txt

doc: doc/DocXul.html

interpreter: $(GRAMMAR_FILES) src/Main.hs src/Interpreter.hs src/TypeChecker.hs
	ghc src/Main.hs -O2 -isrc/ -ibuild_bnfc/ -o interpreter

run_good: interpreter
	@for f in good/*.xul; do \
		echo "$$f"; \
		./interpreter "$$f"; \
	done

run_bad: interpreter
	@for f in bad/*.xul; do \
		echo "$$f"; \
		./interpreter "$$f"; \
		echo; \
	done

zip: clean
	mkdir -p lukasz_raszkiewicz/; \
	cp -r bad/ good/ src/ Makefile README lukasz_raszkiewicz; \
	zip lukasz_raszkiewicz.zip -r lukasz_raszkiewicz/

clean:
	rm -rf build_bnfc/ build_bnfc_stack/ doc/ dist/ lukasz_raszkiewicz/ src/*.o src/*.hi interpreter lukasz_raszkiewicz.zip
