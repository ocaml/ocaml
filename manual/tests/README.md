These tests have for objective to test the consistency between the manual and
the rest of the compiler sources:

- `cross_reference_checker.ml` checks that reference to the manual from the
  compiler sources are still accurate.

- `check-stdlib-modules` checks that all stdlib modules are linked from the
  main entry of the stdlib in the manual:
  `manual/src/library/stdlib-blurb.etex`
