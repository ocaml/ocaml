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
sharedldtype='%%SHAREDLDTYPE%%'
dynlink='%%SUPPORTS_SHARED_LIBRARIES%%'
custom_opt='-custom'
failsafe='false'

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
        caml_libs="$caml_libs $1 $2"
        shift;;
    -ccopt)
        caml_opts="$caml_opts $1 $2"
        shift;;
    -custom)
        dynlink=false;;
    -I)
        caml_opts="$caml_opts $1 $2"
        shift;;
    -failsafe)
        failsafe=true;;
    -linkall)
        caml_opts="$caml_opts $1";;
    -l*)
        c_libs="$c_libs $1"
        c_libs_caml="$c_libs_caml -cclib $1";;
    -L*)
        c_opts="$c_opts $1"
        c_opts_caml="$c_opts_caml -ccopt $1";;
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
    -pthread)
        c_opts_caml="$c_opts_caml -ccopt $1";;
    -R|-rpath)
        c_opts="$c_opts $1 $2"
        c_opts_caml="$c_opts_caml -ccopt $1 -ccopt $2"
        shift;;
    -R*)
        c_opts="$c_opts $1"
        c_opts_caml="$c_opts_caml -ccopt $1";;
    -Wl,-rpath)
        case $2 in
        -Wl,*)
            rpatharg=`echo $2 | sed "s/^-Wl,//"`
            if test "$sharedldtype" = "ld"; then
                c_opts="$c_opts -rpath $rpatharg"
            else
                c_opts="$c_opts $1,$rpatharg"
            fi
            c_opts_caml="$c_opts_caml -ccopt $1,$rpatharg"
            shift;;
        *)
            echo "No argument to '$1', ignored" 1>&2;;
        esac;;
    -Wl,-rpath,*)
        if test "$sharedldtype" = "ld"; then
            rpatharg=`echo $1 | sed "s/^-Wl,-rpath,//"`
            c_opts="$c_opts -rpath $rpatharg"
        else
            c_opts="$c_opts $1"
        fi
        c_opts_caml="$c_opts_caml -ccopt $1";;
    -Wl,-R*)
        if test "$sharedldtype" = "ld"; then
            rpatharg=`echo $1 | sed "s/^-Wl,-R//"`
            c_opts="$c_opts -R$rpatharg"
        else
            c_opts="$c_opts $1"
        fi
        c_opts_caml="$c_opts_caml -ccopt $1";;
    -*)
        echo "Unknown option '$1', ignored" 1>&2;;
    *)
        echo "Don't know what to do with '$1', ignored" 1>&2;;
    esac
    shift
done

if test "$output_c" = ""; then output_c="$output"; fi

set -e

if test "$c_objs" != ""; then
    if $dynlink; then
        %%MKSHAREDLIB%% lib$output_c.so $c_objs $c_opts $c_libs || $failsafe
    fi
    rm -f lib$output_c.a
    ar rc lib$output_c.a $c_objs
    %%RANLIB%% lib$output_c.a
fi
if $dynlink && test "$failsafe" = "false" || test -f lib$output_c.so; then
    c_libs_caml=''
    custom_opt=''
fi
if test "$bytecode_objs" != ""; then
     $ocamlc -a $custom_opt -o $output.cma $caml_opts $bytecode_objs \
        -cclib -l$output_c $caml_libs $c_opts_caml $c_libs_caml
fi
if test "$native_objs" != ""; then
    $ocamlopt -a -o $output.cmxa $caml_opts $native_objs \
        -cclib -l$output_c $caml_libs $c_opts_caml $c_libs_caml
fi

