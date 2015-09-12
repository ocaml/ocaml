# How to contribute

:+1::tada: First off, thank you for taking time to contribute! :tada::+1:

The following is a set of guidelines for contributing to OCaml. These
are just guidelines, not rules, use your best judgment and feel free
to propose changes to this document in a pull request.

## Coding Guidelines

* indentation: particular ocp-indent configuration?
* first match pattern with/without `|`?
* CamelCase? snake_case?

## Submitting a Pull Request


* Add new tests.
 * Merge request for bug-fix  must contain tests which fail before
   application of the fix and not after. The best way, if possible, is
   to add the test in one commit before the fix with an output
   reference file which shows the error. Then in another commit add
   the fix and update the reference file. Doing so help to see what
   is the effect of the fix.
 * Merge request adding a new feature must add tests for normal and
   wrong uses of the feature.

* You should run all the tests before creating the merge request or
  pushing new commits (even if travis will also do it for you): `make
  tests`.

* The description of the merge request must contain a precise
  explanation of the new feature and a proposition to modify the
  ocaml manual.

* Try to split commit into logical units. For example:
  * If a modification requires refactoring of existing code, the
    refactoring and the modification should be done in two different
    commits.
  * Optimizations should go in specific commits, and so keep simple
    a previous commit that adds a functionality.

* Contributor License Agreement?

* Changelog?
