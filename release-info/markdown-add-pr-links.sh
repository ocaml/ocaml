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

# This script performs a series of transformation on its argument to
# turn ASCII references into Markdown-format links:
# - #NNNN links to Github
# - (Changes#VERSION) link to the Changes file
# Breaking change list bullet are converted into annotations

# It was only tested with GNU sed. Sorry!

GITHUB=https://github.com/ocaml/ocaml

sed "s,(Changes#\(.*\)),[Changes file for \\1]($GITHUB/blob/\\1/Changes),g" $1 \
| sed "s,#\([0-9]\+\),[#\\1]($GITHUB/issues/\\1),g" \
| sed "s/^*/* [*breaking change*]/g"
