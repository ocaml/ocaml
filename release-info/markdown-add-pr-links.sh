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

# Note: we turn #1234 into #[1234](.../issues/1234), with the '#'
# outside the link. This gives the property that the script is
# idempotent, it can be run on its own output and keeps it
# unchanged. This makes it possible, for example, to run the script,
# then add yet more non-linkified content, and run the script again on
# the whole file.

GITHUB=https://github.com/ocaml/ocaml

# Note: "cat $1 | sed ..." could be "sed ... $1", but this form makes
# it easier to remove or reorder some of the sed passes.
cat $1 \
| sed "s,(Changes#\(.*\)),[Changes file for \\1]($GITHUB/blob/\\1/Changes),g" \
| sed "s,#\([0-9]\+\),#[\\1]($GITHUB/issues/\\1),g" \
| sed "s/^*/* [*breaking change*]/g"
