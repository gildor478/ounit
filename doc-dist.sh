#!/bin/bash
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

. admin-gallu-common || exit 1

set -e

arg_string_set version --default "dev" \
  "Version of OUnit."

arg_parse arg_anon_fail "$@"

CURDIR=$(pwd)

TOPDIR="ounit-doc-$version"
get_tmpdir TEMPDIR
mkdir -p "$TEMPDIR/$TOPDIR/api-ounit"
cp -R _build/src/api-ounit.docdir/* "$TEMPDIR/$TOPDIR/api-ounit"

tar czf "$CURDIR/dist/$TOPDIR.tar.gz" -C $TEMPDIR $TOPDIR
