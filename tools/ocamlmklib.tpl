#!/bin/sh
#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#           Xavier Leroy, projet Cristal, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 2001 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

bytecode_objs=''
native_objs=''
c_objs=''
c_libs=''
c_libs_caml=''
c_opts=''
c_opts_caml=''
caml_opts=''
caml_libs=''
ocamlc='%%BINDIR%%/ocamlc'
ocamlopt='%%BINDIR%%/ocamlopt'
output='a'
output_c=''

while :; do
    case "$1" in
    "")
        break;;
    *.cmo|*.cma)
        bytecode_objs="$bytecode_objs $1";;
    *.cmx|*.cmxa)
        native_objs="$native_objs $1";;
    *.ml|*.mli)
        bytecode_objs="$bytecode_objs $1"
        native_objs="$native_objs $1";;
    *.o|*.a)
        c_objs="$c_objs $1";;
    -cclib)
        caml_libs="$caml_libs $1 $2";;
    -l*)
        c_libs="$c_libs $1"
        c_libs_caml="$c_libs_caml -cclib $1";;
    -L*)
        c_opts="$c_opts $1"
        c_opts_caml="$c_opts_caml -ccopt $1";;
    -I)
        caml_opts="$caml_opts $1 $2"
        shift;;
    -linkall)
        caml_opts="$caml_opts $1"
        shift;;
    -ocamlc)
        ocamlc="$2"
        shift;;
    -ocamlopt)
        ocamlopt="$2"
        shift;;
    -o)
        output="$2"
        shift;;
    -oc)
        output_c="$2"
        shift;;
    -*)
        echo "Unknown option '$1', ignored" 1>&2;;
    *)
        echo "Don't know what to do with '$1', ignored" 1>&2;;
    esac
    shift
done

if test "$output_c" = ""; then output_c="$output"; fi

set -e

if %%SUPPORTS_SHARED_LIBRARIES%%; then
    if test "$bytecode_objs" != ""; then
        $ocamlc -a -o $output.cma $caml_opts $bytecode_objs -cclib -l$output_c $caml_libs
    fi
    if test "$native_objs" != ""; then
        $ocamlopt -a -o $output.cmxa $caml_opts $native_objs -cclib -l$output_c $caml_libs
    fi
    if test "$c_objs" != ""; then
        %%MKSHAREDLIB%% lib$output_c.so $c_objs $c_opts $c_libs
        rm -f lib$output_c.a
        ar rc lib$output_c.a $c_objs
        %%RANLIB%% lib$output_c.a
    fi
else
    if test "$bytecode_objs" != ""; then
        $ocamlc -a -custom -o $output.cma $caml_opts $bytecode_objs \
            -cclib -l$output_c $caml_libs $c_opts $c_libs
    fi
    if test "$native_objs" != ""; then
        $ocamlopt -a -o $output.cmxa $caml_opts $native_objs \
            -cclib -l$output_c $caml_libs $c_opts $c_libs
    fi
    if test "$c_objs" != ""; then
        rm -f lib$output_c.a
        ar rc lib$output_c.a $c_objs
        %%RANLIB%% lib$output_c.a
    fi
fi
