#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Gabriel Scherer, projet Parsifal, INRIA Saclay              *
#*                                                                        *
#*   Copyright 2018 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# This script performs a series of transformation on standard input to
# turn ASCII references into Markdown-format links:
# - GPR#NNNN links to Github
# - MPR#NNNN and PR#NNNN link to Mantis
# - (Changes#VERSION) link to the Changes file

# It was only tested with GNU sed. Sorry!

GITHUB=https://github.com/ocaml/ocaml
MANTIS=https://caml.inria.fr/mantis

cat \
| sed "s,GPR#\\([0-9]*\\),[GPR~#~\\1]($GITHUB/pull/\\1),g"\
| sed "s,MPR#\\([0-9]*\\),[PR~#~\\1]($MANTIS/view.php?id=\\1),g"\
| sed "s,PR#\\([0-9]*\\),[PR~#~\\1]($MANTIS/view.php?id=\\1),g"\
| sed "s,(Changes#\\(.*\\)),[Changes file for \\1]($GITHUB/blob/\\1/Changes),g"\
| sed "s,PR~#~,PR#,g" \
