
OPT = -use-ocamlfind -classic-display
TARGETS = src/bender.cma src/bender.cmxa src/bender.cmxs \
	  src/clients/echo.native src/clients/database.native \
	  src/clients/scietag.native src/utils/scietag_scan.native

all:
	ocamlbuild $(OPT) $(TARGETS)

clean:
	ocamlbuild -clean
watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make ; \
	done

.PHONY: all clean
