
## Announcing a release candidate:

```
Dear OCaml users,

The release of OCaml version $MAJOR.$MINOR.$BUGFIX is imminent.  We have
created a release candidate that you can test.

The source code is available at these addresses:

 https://github.com/ocaml/ocaml/archive/$VERSION.tar.gz
 https://caml.inria.fr/pub/distrib/ocaml-$BRANCH/ocaml-$VERSION.tar.gz

The compiler can also be installed as an OPAM switch with one of the
following commands:

opam update
opam switch create ocaml-variants.$VERSION --repositories=default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git

or

opam update
opam switch create ocaml-variants.$VERSION+<VARIANT> --repositories=default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git

 where you replace <VARIANT> with one of these:
   afl
   flambda
   fp
   fp+flambda

We want to know about all bugs. Please report them here:
 https://github.com/ocaml/ocaml/issues

Happy hacking,

-- $HUMAN for the OCaml team.

<< insert the relevant Changes section >>
```
