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

0. Install the OCaml distribution.

1. Run `make` in the manual.

NB: If you already set `LD_LIBRARY_PATH` (OS X: `DYLD_LIBRARY_PATH`)
 in you environnement don't forget to add
 `otherlibs/unix:otherlibs/str` to it in an absolute way.

Outputs
-------

In the manual:

- The HTML Manual is in directory `htmlman`. The main file is `index.html`.

- The plain text manual is in direcory `textman` as file `manual.txt`.

- The Info manual is in directory `infoman`.

- The DVI manual is in directory `texstuff` as file `manual.dvi`.

- The PDF manual is in directory `texstuff` as file `pdfmanual.pdf`.

