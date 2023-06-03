OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

LEXSRC  := $(wildcard src/*.mll)
YACCSRC := $(wildcard src/*.mly)

SRC   := $(wildcard src/*.ml)
SRC   := $(filter-out $(LEXSRC:%.mll=%.ml), $(SRC))
SRC   := $(filter-out $(YACSRC:%.mly=%.ml), $(SRC))
SRC   := $(SRC) $(YACSRC:%.mly=%.ml) $(LEXSRC:%.mll=%.ml)
TESTS := $(wildcard tests/*.ml)
TOOLS := $(wildcard tools/*.ml)
BIN   := $(SRC:%.ml=%.cmx)

CLEAN := $(BIN) $(SRC:%.ml=%.cmi) $(SRC:%.ml=%.o)
CLEAN := $(CLEAN) $(TESTS:%.ml=%.cmx) $(TESTS:%.ml=%.cmi) $(TESTS:%.ml=%.o)
CLEAN := $(CLEAN) $(TOOLS:%.ml=%.cmx) $(TOOLS:%.ml=%.cmi) $(TOOLS:%.ml=%.o)
CLEAN := $(CLEAN) $(LEXSRC:%.mll=%.ml) $(YACCSRC:%.mly=%.ml) $(LEXSRC:%.mll=%.mli) $(YACCSRC:%.mly=%.mli)

BIN := $(filter-out src/main.cmx, $(BIN))


%.cmx: %.ml
	if [ -s $<i ]; then $(OCAMLC) -c $<i -I src; fi
	$(OCAMLOPT) -c $< -I src


# creates the bin folder so that it can create binaries inside
./bin:
	mkdir bin


gen-parser: $(YACCSRC) $(LEXSRC)
	for yacc in $(YACCSRC);     \
	do $(OCAMLYACC) $$yacc;	    \
	done

	for lex in $(LEXSRC);       \
	do $(OCAMLLEX) $$lex;       \
	done


build: gen-parser $(BIN) src/main.cmx ./bin
	$(OCAMLOPT) -o bin/lambda-calc $(BIN) src/main.cmx


test-%: tests/t%.cmx $(BIN) ./bin
	$(OCAMLOPT) -o bin/$@ $(BIN) $<
	# $(OCAMLOPT) -o bin/$@ unix.cmxa $(BIN) $<


tool-%: tools/%.cmx $(BIN) ./bin
	$(OCAMLOPT) -o bin/$@ $(BIN) $<
	# $(OCAMLOPT) -o bin/$@ unix.cmxa $(BIN) $<


clean:
	rm $(CLEAN)
