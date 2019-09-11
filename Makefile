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

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

.PHONY: build doc test all install uninstall clean

doc-test: doc
	 ocamldoc -g ../ocaml-tmp/odoc-extract-code/odoc_extract_code.cmo \
	   -load _build/src/oUnit.odoc -intro doc/manual.txt > _build/src/tmp.ml;
	 ocamlc -c -I _build/src/ _build/src/tmp.ml

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

doc-dev-dist: doc fix-perms
	./doc-dist.sh --version dev

.PHONY: doc-dev-dist

deploy: doc fix-perms
	./doc-dist.sh --version $(shell oasis query version)
	admin-gallu-deploy --verbose \
	  --forge_upload --forge_group ounit --forge_user gildor-admin \
	  --forge_extra_file "dist/ounit-doc-$(shell oasis query version).tar.gz"
	admin-gallu-oasis-increment \
	  --setup_run --setup_args "-setup-update dynamic" --use_vcs

.PHONY: deploy

fix-perms:
	chmod +x doc-dist.sh

.PHONY: fix-perms

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name log-html -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs /usr/bin/headache -h _header -c _headache.config

.PHONY: headache
