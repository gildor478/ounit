OUnit - xUnit testing framework for OCaml
=========================================================================

[![Travis status][travis-img]][travis]
[![AppVeyor status][appveyor-img]][appveyor]

OUnit is a unit test framework for OCaml. It allows one to easily create
unit-tests for OCaml code. It is loosely based on [HUnit], a unit testing
framework for Haskell. It is similar to [JUnit], and other XUnit testing
frameworks.

[HUnit]:          http://hunit.sourceforge.net/
[JUnit]:          http://junit.org/
[travis]:         https://travis-ci.org/gildor478/ounit
[travis-img]:     https://travis-ci.org/gildor478/ounit.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/gildor478/ounit
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/g86mhhc0dda25llx/branch/master?svg=true
[opam]:           https://opam.ocaml.org

Installation
------------

The recommended way to install ounit is via the [opam package manager][opam]:

```sh
$ opam install ounit
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
