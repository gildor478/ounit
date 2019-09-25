############################################################################
#  The OUnit library                                                       #
#                                                                          #
#  Copyright (C) 2002-2008 Maas-Maarten Zeeman.                            #
#  Copyright (C) 2010 OCamlCore SARL                                       #
#  Copyright (C) 2013 Sylvain Le Gall                                      #
#                                                                          #
#  The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL   #
#  and Sylvain Le Gall.                                                    #
#                                                                          #
#  Permission is hereby granted, free of charge, to any person obtaining   #
#  a copy of this document and the OUnit software ("the Software"), to     #
#  deal in the Software without restriction, including without limitation  #
#  the rights to use, copy, modify, merge, publish, distribute,            #
#  sublicense, and/or sell copies of the Software, and to permit persons   #
#  to whom the Software is furnished to do so, subject to the following    #
#  conditions:                                                             #
#                                                                          #
#  The above copyright notice and this permission notice shall be          #
#  included in all copies or substantial portions of the Software.         #
#                                                                          #
#  The Software is provided ``as is'', without warranty of any kind,       #
#  express or implied, including but not limited to the warranties of      #
#  merchantability, fitness for a particular purpose and noninfringement.  #
#  In no event shall Maas-Maarten Zeeman be liable for any claim, damages  #
#  or other liability, whether in an action of contract, tort or           #
#  otherwise, arising from, out of or in connection with the Software or   #
#  the use or other dealings in the software.                              #
#                                                                          #
#  See LICENSE.txt for details.                                            #
############################################################################

version = dev

default: test

build:
	dune build @install

doc:
	dune build @doc

test:
	dune runtest

all:
	dune build @all
	dune runtest

install: install-ounit install-ounit-lwt

install-ounit:
	-ocamlfind remove oUnit
	ocamlfind install oUnit src/lib/oUnit/META -patch-version $(version)

install-ounit-lwt:
	-ocamlfind remove ounit-lwt
	ocamlfind install ounit-lwt src/lib/ounit-lwt/META -patch-version $(version)

uninstall:
	dune uninstall
	-ocamlfind remove oUnit
	-ocamlfind remove ounit-lwt

clean:
	dune clean

null:
	true

.PHONY: build doc test all uninstall clean null
.PHONY: install install-ounit install-ounit-lwt

PRECOMMIT_ARGS= \
	    --exclude log-html \
	    --exclude Makefile

precommit:
	 -@if command -v OCamlPrecommit > /dev/null; then \
	   OCamlPrecommit $(PRECOMMIT_ARGS); \
	 else \
	   echo "Skipping precommit checks.";\
	 fi

test: precommit

.PHONY: precommit

deploy: doc test
	dune-release lint
	git push --all
	dune-release tag
	dune-release distrib --skip-tests
	dune-release publish
	dune-release opam pkg
	dune-release opam submit

.PHONY: deploy

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name log-html -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs /usr/bin/headache -h _header -c _headache.config

.PHONY: headache
