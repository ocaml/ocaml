#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2001 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id$

!include ..\config\Makefile.nt

CC=cl /nologo
CFLAGS=/MT -O /Zi

OBJS=startocaml.obj ocaml.res ocaml.obj menu.obj

LIBS=kernel32.lib advapi32.lib gdi32.lib user32.lib comdlg32.lib comctl32.lib

all: ocamlwin.exe

ocamlwin.exe: $(OBJS)
	$(CC) $(CFLAGS) -o ocamlwin.exe $(OBJS) $(LIBS)

ocaml.res: ocaml.rc ocaml.ico
	rc ocaml.rc

$(OBJS): inria.h inriares.h

clean:
	rm -f ocamlwin.exe ocaml.res *.obj *.pdb ocamlwin.ilk

install:
	cp ocamlwin.exe $(PREFIX)/OCamlWin.exe

.SUFFIXES: .c .obj

.c.obj:
	$(CC) $(CFLAGS) -c $*.c
