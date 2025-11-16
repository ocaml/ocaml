TODO for the OCaml Typechecker Implementation
=============================================

There is a consensus that the current implementation of the OCaml
typechecker is overly complex and fragile (*Is this still true?*).  
A big rewrite "from scratch" might be possible or desirable at some point
(or not, see *link to discussion*) but incremental cleanup steps are accessible
and could improve the current implementation at a relatively small cost and in
a reasonable time frame. 

Goals of the cleanup:

 - Make the implementation more maintainable and less fragile.

 - Allow new contributors, or people involved in bigger rewriting
   projects, to get familiar with the code base more easily.

 - Pave the way for future extensions or bigger structural changes to
   the implementation.

This file collects ideas which have been discussed amongst maintainers.  
Having the list committed in the repo allows for everyone to get an idea
of planned tasks, refine them through Pull Requests, suggest more cleanups,
or even start working on specific tasks (ideally after discussing it first
with maintainers).

# Code smells
(*Potentially label these so they can be referenced more easily in discussions
about benefits of proposals?*)

A. Global mutable state

B. Poor data representation

C. Avoid constructing a parsetree locally
  (methods build a piece of AST with a self argument
   with a *-using name to avoid conflicts; #row, etc.)

D. Avoid magic string literals

# TODO List

Not all ideas have been thoroughly discussed, and there might not be a
consensus for all of them.

1. Make the level generator be part of `Env.t` instead of being global.

2. Introduce an abstraction boundary between "the type algebra" and
  "the type checker". 

3. Collect all global state of the type checker in a single place,
  possibly a single reference to a persistent data structure
  (e.g. maps instead of hashtables).

4. Consider storing warning settings (+other context) as part of `Env.t`?

5. - Document row_desc
   - get rid of row_bound.

6. Implement union-find with a more abstract/persistent datastructure

7. Make the logic for record/constructor disambiguation more readable.

8. Tidy up destructive substitution.

9. Get rid of syntactic encodings (generating Parsetree fragments
  during type-checking, cf optional arguments or classes).

10. Track "string literals" in the type-checker, which often act as
  magic "internal" names which should be avoided.

11. Use a map to remove `Tsubst`.

12. Parse attributes understood (e.g. the deprecated attribute) by the
  compiler into a structured representation during type-checking.

13. Introduce a notion of syntactic "path-like location" to point to
  allow pointing to AST fragments, and use that to implement "unused"
  warnings in a less invasive and less imperative way.
  (See Thomas' PR)

14. Deprecate -nolabels (or even get rid of it?)

15. - Monitor coverage of the typechecker implementation while running the
  testsuite
    - expand the testsuite and/or kill dead code in the typechecker
  to increase coverage ratio.

# Completed

- Tidy up Typeclass (use records instead of 14-tuples, avoid
  "#"-encoding, etc)
    - Completed with commit: 3d4393a2023f0dc67213cbb37d914d2103a1ad83

# Further Information and Discussion

2. This could be implemented between Ctype and Typecore as a start, so 
  that the type checker is forced to go through a proper API to access/mutate
  type nodes.  This would make it impossible to "forget" a call
  to `repr` and will allow further changes on the internal representation.

4. With the unique ids on each type node, copying can be implemented 
  rather efficiently using a map.
  `Tsubst` is currently used in the following files: 
  btype.ml, ctype.ml, gprinttyp.ml, out_type.ml, rawprinttyp.ml, subst.ml, 
  typedecl_separability.ml, typedecl_variance.ml, typeopt.ml, types.ml, 
  types.mli, types.mli

6. We need to be careful about memory leaks with the naive approach of
  representing links with a persistent heap.
  Modest version of the proposal: have an explicit indirection layer
  (type_expr Unode.t) for nodes in the union-find structure. 
  Efficiency cost?

7. Request for Jacques to write a specification which could be used to
  to make the implementation easier for others to understand.

13. Re. see Thomas's PR, maybe this commit? 3762abea10a3c3c7614b418b42d0ed308b8e3693

14. First step is to turn on the warning by default. We could even stop 
  supporting unlabeled full applications?
  Link to any discussion threads that have been had about this: 

15. See PR#8874. Accomplish this by using bisect_ppx. 
  Ask Florian Angeletti and Sebastien Hinderer about the current state
  as it was partially during Oxana's Outreachy internship:
  Maybe commits: 
  7087e24fe45cdcb071450a48ef70bc5500b1c082 
  3e728c389922237bf2e2a20e23514ac78f8bd43e

# Relevant Talks/Publications
* ICFP 2025, A Tale of Two Lambdas: A Haskeller's Journey into OCaml, Richard A. Eisenberg
  - https://conf.researchr.org/details/icfp-splash-2025/haskellsymp-2025-papers/2/-A-Tale-of-Two-Lambdas-A-Haskeller-s-Journey-into-OCaml
  - 3:39:40, https://www.youtube.com/watch?v=IlQQElKaFvM&t=17691s 
  
