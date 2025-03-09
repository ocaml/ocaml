#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                          Samuel Hym, Tarides                           *
#*                                                                        *
#*   Copyright 2023 Tarides                                               *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Awk script to remove the differences between the disassembly of amd64.o as
# assembled by MinGW GCC and amd64nt.obj as assembled by MASM
#
# The main differences are:
# - the encoding of some instructions is not the same even when they have the
#   same mnemonic, so we process the result of `dumpbin /disasm:nobytes`
# - some internal labels are exposed as such on one output, not on the other: so
#   we process the file twice, the first pass to record the position of all
#   internal labels (labels that don't start with `caml`), the second to replace
#   them by their offset in `lea` instructions
# - the final padding is different, with some `nop`s at the
#   `caml_system__code_end` label in one output, not the other, so we drop
#   everything after that label
#
# This script expects the input to be given twice:
#   awk -f me input.dump input.dump
# where the first input.dump is used for the first pass (recording label
# positions) and printing nothing, the second input.dump is for the second pass
# that actually prints the canonicalized assembly, with only the exported labels

BEGIN {
  # first pass (or second pass)
  first=1
  # to skip the final padding
  skip=0
  # the label that appeared on the previous line when we want to record it
  label=""
}
ENDFILE {
  # start the second pass
  first=0
}

# On label lines
/^[a-zA-Z0-9_]*:\s*$/ {
  # In that an internal label?
  if ($0 !~ /^caml/) {
    if (first) {
      sub(/:.*$/, "")
      label=$0
    }
  } else {
    if (!first) {
      if($0 ~ /^caml_system__code_end/) {
        # Drop everything after caml_system__code_end
        skip=1
      } else
        print
    }
  }
}

# On mnemonic lines
/^  [0-9A-F]*:/ {
  # if we are on the first instruction after an internal label, we associate the
  # label to the current position
  if (label != "") {
    sub(/^0*/,"",$1)
    sub(/:$/,"",$1)
    labels[label] = $1
    label=""
  } else {
    if(!first) {
      if ($2 == "lea") {
        for(g in labels) {
          sub(g,labels[g] "h")
        }
      }
      if(!skip)
        print
    }
  }
}
