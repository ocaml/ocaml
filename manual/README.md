OCAML DOCUMENTATION
===================

Prerequisites
-------------

- Any prerequisites required to build OCaml from sources.

- The Unix editor 'ed', no longer installed by default on some systems.

- A LaTeX installation.

- The HeVeA LaTeX-to-HTML convertor (available in OPAM):
  <http://hevea.inria.fr/>

Note that you must make sure `hevea.sty` is installed into TeX properly. Your
package manager may not do this for you. Run `kpsewhich hevea.sty` to check.


Building
--------

1. Check out the relevant OCaml sources from the subversion repository, and
build as usual. If you don't have an OCaml installation, you can install this
one with `make install`.

Subversion: <http://caml.inria.fr/svn/ocaml>

Github mirror: <http://github.com/ocaml/ocaml>

We shall refer to this the "compiler".

2. Check out `ocamldoc` from the subversion repository (contains this
`README` file).

Subversion: <http://caml.inria.fr/svn/ocamldoc>

Github mirror: <http://github.com/ocaml/ocaml-manual>

We shall refer to this as the "manual".

3. Make a symlink `release` in the root directory of the manual
checkout, pointing to the compiler directory from step 1.

4. Make sure `LD_LIBRARY_PATH` (OS X: `DYLD_LIBRARY_PATH`) points to `dllunix.so` and
`dllcamlstr.o` from `ocaml/otherlibs/{unix,str}` in the previously-built compiler
directory

5. Run `make tools` in the manual to build the tools needed for making the
manual

6. Run `make` in the manual.


Outputs
-------

In the manual:

- The HTML Manual is in directory `htmlman`. The main file is `index.html`.

- The plain text manual is in direcory `textman` as file `manual.txt`.

- The Info manual is in directory `infoman`.

- The DVI manual is in directory `texstuff` as file `manual.dvi`.

- The PDF manual is in directory `texstuff` as file `pdfmanual.pdf`.

