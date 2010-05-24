#
# $Id: Makefile,v 1.12 2003/08/19 03:57:12 maas Exp $
#

### End of configuration section

OCAMLRUN=ocamlrun
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
MKLIB=ocamlmklib
OCAMLDOC=ocamldoc
OCAMLFIND=ocamlfind

SOURCES=oUnit.mli oUnit.ml test_OUnit.ml

all: oUnit.cma 
allopt: oUnit.cmxa

oUnit.cma: oUnit.cmi oUnit.cmo
	$(OCAMLC) -a -o oUnit.cma oUnit.cmo
oUnit.cmxa: oUnit.cmi oUnit.cmx
	$(OCAMLOPT) -a -o oUnit.cmxa oUnit.cmx

test: unittest
	./unittest

testopt: unittest.opt
	./unittest.opt

testall: test testopt

unittest: oUnit.cmo test_OUnit.cmo
	$(OCAMLFIND) ocamlc -o unittest -package unix -linkpkg ./oUnit.cmo ./test_OUnit.cmo

unittest.opt: oUnit.cmx test_OUnit.cmx
	$(OCAMLFIND) ocamlopt -o unittest.opt -package unix -linkpkg ./oUnit.cmx ./test_OUnit.cmx

install: oUnit.cmi oUnit.cma oUnit.cmxa
	$(OCAMLFIND) install oUnit META oUnit.mli oUnit.cmi oUnit.cma oUnit.cmxa oUnit.a
uninstall:
	$(OCAMLFIND) remove oUnit

doc: FORCE
	cd doc; $(OCAMLDOC) -html -I .. ../oUnit.mli

FORCE:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $(COMPFLAGS) $<
.ml.cmo:
	$(OCAMLC) -c $(COMPFLAGS) $<
.ml.cmx:
	$(OCAMLOPT) -c $(COMPFLAGS) $<
.c.o:
	$(OCAMLC) -c -ccopt "$(CFLAGS)" $<
clean::
	rm -f *~ *.cm* *.o *.a *.so unittest unittest.opt
.depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend
include .depend

