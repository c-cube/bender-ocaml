
OPT = -use-ocamlfind -classic-display
TARGETS = src/bender.cma src/bender.cmxa src/bender.cmxs src/echo.native

all:
	ocamlbuild $(OPT) $(TARGETS)

clean:
	ocamlbuild -clean

.PHONY: all clean
