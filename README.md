OUnit - xUnit testing framework for OCaml
=========================================================================

[![GitHub Actions][gha-badge]][gha]

OUnit is a unit test framework for OCaml. It allows one to easily create
unit-tests for OCaml code. It is loosely based on [HUnit], a unit testing
framework for Haskell. It is similar to [JUnit], and other XUnit testing
frameworks.

Features:
- colored output
- JUnit report generation
- HTML report generation

[HUnit]:          https://hunit.sourceforge.net/
[JUnit]:          https://junit.org/
[gha]:            https://github.com/gildor478/ounit/actions/workflows/main.yml
[gha-badge]:      https://github.com/gildor478/ounit/actions/workflows/main.yml/badge.svg
[opam]:           https://opam.ocaml.org

Installation
------------

The recommended way to install ounit is via the [opam package manager][opam]:

```sh
$ opam install ounit2
```

Documentation
-------------

API documentation is
[available online](https://gildor478.github.io/ounit).

Examples
--------

* From the examples/ directory of ounit:
  * [test_list.ml](examples/test_list.ml)
  * [test_stack.ml](examples/test_stack.ml)
* External projects:
  * [OASIS tests](https://github.com/ocaml/oasis/tree/master/test)

Transition to ounit2
--------------------

In the past OUnit used the ocamlfind package name "oUnit". It is uncommon to
use uppercase letters in ocamlfind package name. It caused some problems during
the transition to "dune". It was also not the same name as the OPAM package. As
of version 2.2, the opam package ounit and the ocamlfind package oUnit are
renamed to ounit2 (the same for both the ocamlfind and opam packages).

To do the transition for your own tests:
* in OPAM, the library should now depends on "ounit2" or "ounit2-lwt"
* in dune files/OASIS/Makefile/pkg.ml replace "oUnit" by "ounit2" and
  "ounit-lwt" to "ounit2-lwt".

We will keep OPAM packages "ounit"/"ounit-lwt" for the transition.
