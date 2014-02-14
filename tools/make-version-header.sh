#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#          Damien Doligez, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2003 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  As an exception to the       #
#   licensing rules of OCaml, this file is freely redistributable,      #
#   modified or not, without constraints.                               #
#                                                                       #
#########################################################################

# For maximal compatibility with older versions, we Use "ocamlc -v"
# instead of "ocamlc -vnum" or the VERSION file in .../lib/ocaml/.

# This script extracts the components from an OCaml version number
# and provides them as C defines:
# OCAML_VERSION_MAJOR: the major version number
# OCAML_VERSION_MAJOR: the minor version number
# OCAML_VERSION_PATCHLEVEL: the patchlevel number if present, or 0 if absent
# OCAML_VERSION_ADDITIONAL: this is defined only if the additional-info
#  field is present, and is a string that contains that field.
# Note that additional-info is always absent in officially-released
# versions of OCaml.

version="`ocamlc -v | sed -n -e 's/.*version //p'`"

major="`echo "$version" | sed -n -e '1s/^\([0-9]*\)\..*/\1/p'`"
minor="`echo "$version" | sed -n -e '1s/^[0-9]*\.\([0-9]*\).*/\1/p'`"
patchlvl="`echo "$version" | sed -n -e '1s/^[0-9]*\.[0-9]*\.\([0-9]*\).*/\1/p'`"
suffix="`echo "$version" | sed -n -e '1s/^[^+]*+\(.*\)/\1/p'`"

echo "#define OCAML_VERSION_MAJOR $major"
echo "#define OCAML_VERSION_MINOR $minor"
case $patchlvl in "") patchlvl=0;; esac
echo "#define OCAML_VERSION_PATCHLEVEL $patchlvl"
case "$suffix" in
  "") echo "#undef OCAML_VERSION_ADDITIONAL";;
  *) echo "#define OCAML_VERSION_ADDITIONAL \"$suffix\"";;
esac
