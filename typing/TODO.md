TODO for the OCaml typechecker implementation
=============================================

There is a consensus that the current implementation of the OCaml
typechecker is overly complex and fragile.  A big rewriting "from
scratch" might be possible or desirable at some point, or not, but
incremental cleanup steps are certainly accessible and could bring the
current implementation in a better shape at a relatively small cost
and in a reasonably distant future.

Goals of the cleanup:

 - Make the implementation more maintainable and less fragile.

 - Allow new contributors, or people involved in bigger rewriting
   projects, to get familiar with the code base more easily.

 - Pave the way for future extensions or bigger structural changes to
   the implementation.

This file collects specific cleanup ideas which have been discussed
amongst maintainers.  Having the list committed in the repo allows for
everyone to get an idea of planned tasks, refine them through Pull
Requests, suggest more cleanups, or even start working on specific
tasks (ideally after discussing it first with maintainers).

Not all ideas have been thoroughly discussed, and there might not be a
consensus for all of them.

- Make the level generator be part of `Env.t` instead of being global.

- Introduce an abstraction boundary between "the type algebra" and
  "the type checker" (at first between Ctype and Typecore) so that the
  type checker is forced to go through a proper API to access/mutate
  type nodes.  This would make it impossible to "forget" a call
  to `repr` and will allow further changes on the internal representation.

- Tidy up Typeclass (use records instead of 14-tuples, avoid
  "#"-encoding, etc).

- Collect all global state of the type checker in a single place,
  possibly a single reference to a persistent data structure
  (e.g. maps instead of hashtables).

- Get rid of Tsubst.  With the unique ids on each type node, copying
  can be implemented rather efficiently with a map.

- Document row_desc, get rid of row_bound.

- Implement union-find with a more abstract/persistent datastructure
  (be careful about memory leaks with the naive approach of representing
  links with a persistent heap).

- Make the logic for record/constructor disambiguation more readable.

- Tidy up destructive substitution.

- Get rid of syntactic encodings (generating Parsetree fragments
  during type-checking, cf optional arguments or classes).

- Track "string literals" in the type-checker, which often act as
  magic "internal" names which should be avoided.

- Get rid of -annot.

- Consider storing warning settings (+other context) as part of `Env.t`?

- Parse attributes understood (e.g. the deprecated attribute) by the
  compiler into a structured representation during type-checking.

- Introduce a notion of syntactic "path-like location" to point to
  allow pointing to AST fragments, and use that to implement "unused"
  warnings in a less invasive and less imperative way.

- Deprecate -nolabels, or even get rid of it?

- Using e.g. bisect_ppx, monitor coverage of the typechecker
  implementation while running the testsuite, and expand the testsuite
  and/or kill dead code in the typechecker to increase coverage ratio.
