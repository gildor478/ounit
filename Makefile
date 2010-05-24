#
# $Id: Makefile,v 1.13 2003/09/19 21:10:22 maas Exp $
#

OBJECTS=oUnit.cmo
XOBJECTS=oUnit.cmx
TEST_OBJECTS=test_OUnit.cmo
TEST_XOBJECTS=test_OUnit.cmx
ARCHIVE=oUnit.cma
XARCHIVE=oUnit.cmxa
NAME=oUnit

### End of configuration section

OCAMLRUN=ocamlrun
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
MKLIB=ocamlmklib
OCAMLDOC=ocamldoc
OCAMLFIND=ocamlfind

all: $(ARCHIVE)
allopt: $(XARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) $(OBJECTS)
$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) $(XOBJECTS)

depend: *.ml *.mli
	$(OCAMLDEP) *.mli *.ml > depend

test: unittest
	./unittest

testopt: unittest.opt
	./unittest.opt

testall: test testopt

unittest: $(OBJECTS) $(TEST_OBJECTS)
	$(OCAMLFIND) ocamlc -o unittest -package unix -linkpkg $(OBJECTS) $(TEST_OBJECTS)

unittest.opt: $(XOBJECTS) $(TEST_XOBJECTS)
	$(OCAMLFIND) ocamlopt -o unittest.opt -package unix -linkpkg $(XOBJECTS) $(TEST_XOBJECTS)

.PHONY: install
install: all
	{ test ! -f $(XARCHIVE) || extra="$(XARCHIVE) $(NAME).a"; }; \
	$(OCAMLFIND) install $(NAME) META $(NAME).mli $(NAME).cmi $(ARCHIVE) $$extra

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove $(NAME)

.PHONY: doc
doc: FORCE
	{ test -d doc || mkdir doc; };
	cd doc; $(OCAMLDOC) -html -I .. ../$(NAME).mli

.PHONY: clean
clean::
	rm -f *~ *.cm* *.o *.a *.so unittest unittest.opt doc/*.html doc/*.css

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

include depend

