#(***********************************************************************)
#(*                            OCamldoc                                 *)
#(*                                                                     *)
#(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
#(*                                                                     *)
#(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
#(*  en Automatique.  All rights reserved.  This file is distributed    *)
#(*  under the terms of the Q Public License version 1.0.               *)
#(*                                                                     *)
#(***********************************************************************)

# $Id$

include ../config/Makefile

# Various commands and dir
##########################
CAMLRUN=../boot/ocamlrun
OCAMLC   = $(CAMLRUN) ../boot/ocamlc -I ../boot -warn-error A
OCAMLOPT = $(CAMLRUN) ../ocamlopt 
OCAMLDEP = $(CAMLRUN) ../tools/ocamldep
OCAMLLEX = $(CAMLRUN) ../boot/ocamllex
OCAMLYACC= ../boot/ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)
EXTRAC_CRC = $(CAMLRUN) ../otherlibs/dynlink/extract_crc

OCAMLPP=-pp 'grep -v DEBUG'

# For installation 
##############
MKDIR=mkdir -p
CP=cp -f
OCAMLDOC=ocamldoc
OCAMLDOC_OPT=$(OCAMLDOC).opt
OCAMLDOC_LIBCMA=odoc_info.cma 
OCAMLDOC_LIBCMI=odoc_info.cmi
OCAMLDOC_LIBCMXA=odoc_info.cmxa 
OCAMLDOC_LIBA=odoc_info.a
INSTALL_LIBDIR=$(OCAMLLIB)/ocamldoc
INSTALL_BINDIR=$(OCAMLBIN)

INSTALL_MLIS=odoc_info.mli
INSTALL_CMIS=$(INSTALL_MLIS:.mli=.cmi)

# Compilation
#############
OCAMLSRCDIR:=..
INCLUDES=  -I $(OCAMLSRCDIR)/parsing \
	-I $(OCAMLSRCDIR)/utils \
	-I $(OCAMLSRCDIR)/typing \
	-I $(OCAMLSRCDIR)/driver \
	-I $(OCAMLSRCDIR)/bytecomp \
	-I $(OCAMLSRCDIR)/tools \
	-I $(OCAMLSRCDIR)/stdlib \
	-I $(OCAMLSRCDIR)/otherlibs/str \
	-I $(OCAMLSRCDIR)/otherlibs/dynlink \
	-I $(OCAMLSRCDIR)/otherlibs/unix \
	-I $(OCAMLSRCDIR)/otherlibs/num \
	-I $(OCAMLSRCDIR)/otherlibs/graph \
	-I $(OCAMLSRCDIR)/toplevel/ \

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

CMOFILES= odoc_global.cmo\
	odoc_messages.cmo\
	odoc_types.cmo\
	odoc_misc.cmo\
	odoc_text_parser.cmo\
	odoc_text_lexer.cmo\
	odoc_text.cmo\
	odoc_name.cmo\
	odoc_parameter.cmo\
	odoc_value.cmo\
	odoc_type.cmo\
	odoc_exception.cmo\
	odoc_class.cmo\
	odoc_module.cmo\
	odoc_str.cmo\
	odoc_args.cmo\
	odoc_comments_global.cmo\
	odoc_parser.cmo\
	odoc_lexer.cmo\
	odoc_see_lexer.cmo\
	odoc_comments.cmo\
	odoc_env.cmo\
	odoc_merge.cmo\
	odoc_sig.cmo\
	odoc_ast.cmo\
	odoc_control.cmo\
	odoc_inherit.cmo\
	odoc_search.cmo\
	odoc_cross.cmo\
	odoc_dep.cmo\
	odoc_analyse.cmo\
	odoc_scan.cmo\
	odoc_info.cmo


CMXFILES= $(CMOFILES:.cmo=.cmx)
CMIFILES= $(CMOFILES:.cmo=.cmi)

EXECMOFILES=$(CMOFILES)\
	odoc_dag2html.cmo\
	odoc_to_text.cmo\
	odoc_ocamlhtml.cmo\
	odoc_html.cmo\
	odoc_man.cmo\
	odoc_latex.cmo\
	odoc_dot.cmo\
	odoc_iso.cmo

EXECMXFILES= $(EXECMOFILES:.cmo=.cmx)
EXECMIFILES= $(EXECMOFILES:.cmo=.cmi)

LIBCMOFILES=$(CMOFILES)
LIBCMXFILES= $(LIBCMOFILES:.cmo=.cmx)
LIBCMIFILES= $(LIBCMOFILES:.cmo=.cmi)

# Les cmo et cmx de la distrib OCAML
OCAMLCMOFILES=$(OCAMLSRCDIR)/parsing/printast.cmo \
	$(OCAMLSRCDIR)/typing/ident.cmo \
	$(OCAMLSRCDIR)/utils/tbl.cmo \
	$(OCAMLSRCDIR)/utils/misc.cmo \
	$(OCAMLSRCDIR)/utils/config.cmo \
	$(OCAMLSRCDIR)/utils/clflags.cmo \
	$(OCAMLSRCDIR)/utils/warnings.cmo \
	$(OCAMLSRCDIR)/utils/ccomp.cmo \
	$(OCAMLSRCDIR)/parsing/linenum.cmo\
	$(OCAMLSRCDIR)/parsing/location.cmo\
	$(OCAMLSRCDIR)/parsing/longident.cmo \
	$(OCAMLSRCDIR)/parsing/syntaxerr.cmo \
	$(OCAMLSRCDIR)/parsing/parser.cmo \
	$(OCAMLSRCDIR)/parsing/lexer.cmo \
	$(OCAMLSRCDIR)/parsing/parse.cmo \
	$(OCAMLSRCDIR)/typing/types.cmo \
	$(OCAMLSRCDIR)/typing/path.cmo \
	$(OCAMLSRCDIR)/typing/btype.cmo \
	$(OCAMLSRCDIR)/typing/predef.cmo \
	$(OCAMLSRCDIR)/typing/datarepr.cmo \
	$(OCAMLSRCDIR)/typing/subst.cmo \
	$(OCAMLSRCDIR)/typing/env.cmo \
	$(OCAMLSRCDIR)/typing/ctype.cmo \
	$(OCAMLSRCDIR)/typing/primitive.cmo \
	$(OCAMLSRCDIR)/typing/oprint.cmo \
	$(OCAMLSRCDIR)/typing/printtyp.cmo \
	$(OCAMLSRCDIR)/typing/includecore.cmo \
	$(OCAMLSRCDIR)/typing/typetexp.cmo \
	$(OCAMLSRCDIR)/typing/parmatch.cmo \
	$(OCAMLSRCDIR)/typing/typedtree.cmo \
	$(OCAMLSRCDIR)/typing/typecore.cmo \
	$(OCAMLSRCDIR)/typing/includeclass.cmo \
	$(OCAMLSRCDIR)/typing/typedecl.cmo \
	$(OCAMLSRCDIR)/typing/typeclass.cmo \
	$(OCAMLSRCDIR)/typing/mtype.cmo \
	$(OCAMLSRCDIR)/typing/includemod.cmo \
	$(OCAMLSRCDIR)/typing/typemod.cmo \
	$(OCAMLSRCDIR)/bytecomp/lambda.cmo \
	$(OCAMLSRCDIR)/bytecomp/typeopt.cmo \
	$(OCAMLSRCDIR)/bytecomp/printlambda.cmo \
	$(OCAMLSRCDIR)/bytecomp/switch.cmo \
	$(OCAMLSRCDIR)/bytecomp/matching.cmo \
	$(OCAMLSRCDIR)/bytecomp/translobj.cmo \
	$(OCAMLSRCDIR)/bytecomp/translcore.cmo \
	$(OCAMLSRCDIR)/bytecomp/translclass.cmo \
	$(OCAMLSRCDIR)/tools/depend.cmo

OCAMLCMXFILES=$(OCAMLCMOFILES:.cmo=.cmx)

all: exe lib
exe: $(OCAMLDOC)
lib: $(OCAMLDOC_LIBCMA) $(OCAMLDOC_LIBCMI) 

opt.opt: exeopt libopt
exeopt: $(OCAMLDOC_OPT)
libopt: $(OCAMLDOC_LIBCMXA) $(OCAMLDOC_LIBCMI) 
debug: 
	make OCAMLPP=""

$(OCAMLDOC): $(EXECMIFILES) $(EXECMOFILES) odoc_crc.cmo odoc.cmo
	$(OCAMLC) -o $@ unix.cma str.cma dynlink.cma $(LINKFLAGS) $(OCAMLCMOFILES) $(EXECMOFILES) odoc_crc.cmo odoc.cmo
$(OCAMLDOC_OPT): $(EXECMIFILES) $(EXECMXFILES) odoc_opt.cmx
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa $(LINKFLAGS) $(OCAMLCMXFILES) $(EXECMXFILES) odoc_opt.cmx

$(OCAMLDOC_LIBCMA): $(LIBCMIFILES) $(LIBCMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(OCAMLCMOFILES) $(LIBCMOFILES)
$(OCAMLDOC_LIBCMXA): $(LIBCMIFILES) $(LIBCMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(OCAMLCMXFILES) $(LIBCMXFILES)

odoc_crc.ml: $(CMIFILES)
	$(EXTRAC_CRC) $(INCLUDES)\
	Arg Arith_status Array Big_int Buffer Callback Char Digest Dynlink \
	Filename Format Gc Genlex Graphics Hashtbl \
	Lazy Lexing List Map Marshal Nat\
	Num Obj Oo Outcometree Parsing Pervasives Printexc\
	Printf Profiling Queue Random Ratio\
	Set Sort Stack Std_exit Str Stream\
	String Sys Topdirs Toploop Unix Weak\
	Printast \
	Ident \
	Tbl \
	Misc \
	Config \
	Clflags \
	Warnings \
	Ccomp \
	Linenum\
	Location\
	Longident \
	Syntaxerr \
	Parser \
	Lexer \
	Parse \
	Types \
	Path \
	Btype \
	Predef \
	Datarepr \
	Subst \
	Env \
	Ctype \
	Primitive \
	Oprint \
	Printtyp \
	Includecore \
	Typetexp \
	Parmatch \
	Typedtree \
	Typecore \
	Includeclass \
	Typedecl \
	Typeclass \
	Mtype \
	Includemod \
	Typemod \
	Lambda \
	Typeopt \
	Printlambda \
	Switch \
	Matching \
	Translobj \
	Translcore \
	Bytesections \
	Runtimedef \
	Symtable \
	Opcodes \
	Bytelink \
	Bytelibrarian \
	Translclass \
	Errors \
	Main_args \
	Asttypes \
	Depend \
	Odoc_global Odoc_args Odoc_info Odoc_messages Odoc_types\
	Odoc_misc Odoc_text_parser Odoc_text_lexer\
	Odoc_text Odoc_comments_global Odoc_parser\
	Odoc_lexer Odoc_comments Odoc_name Odoc_parameter\
	Odoc_value Odoc_type Odoc_exception Odoc_class\
	Odoc_module Odoc_str Odoc_args Odoc_env\
	Odoc_sig Odoc_ast Odoc_control Odoc_inherit\
	Odoc_search Odoc_cross Odoc_merge Odoc_analyse\
	Odoc_dag2html Odoc_ocamlhtml Odoc_html\
	Odoc_latex Odoc_man Odoc_iso Odoc_scan > $@

# generic rules :
#################

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

%.cmi:%.mli
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmo:%.ml
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

%.ml:%.mll
	$(OCAMLLEX) $<

%.mli %.ml:%.mly
	$(OCAMLYACC) -v $<

# Installation targets
######################
install: dummy
	if test -d $(INSTALL_BINDIR); then : ; else $(MKDIR) $(INSTALL_BINDIR); fi
	if test -d $(INSTALL_LIBDIR); then : ; else $(MKDIR) $(INSTALL_LIBDIR); fi
	$(CP) $(OCAMLDOC) $(INSTALL_BINDIR)
	$(CP) ocamldoc.sty ocamldoc.hva *.cmi $(GENERATORS) $(OCAMLDOC_LIBCMA) $(INSTALL_LIBDIR)
	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_LIBDIR)

installopt:
	if test -f $(OCAMLDOC_OPT) ; then $(MAKE) installopt_really ; fi

installopt_really:
	if test -d $(INSTALL_BINDIR); then : ; else $(MKDIR) $(INSTALL_BINDIR); fi
	if test -d $(INSTALL_LIBDIR); then : ; else $(MKDIR) $(INSTALL_LIBDIR); fi
	$(CP) $(OCAMLDOC_OPT) $(INSTALL_BINDIR)
	$(CP) ocamldoc.sty ocamldoc.hva $(OCAMLDOC_LIBA) $(OCAMLDOC_LIBCMXA) $(INSTALL_LIBDIR)
	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_LIBDIR)


# backup, clean and depend :
############################

clean:: dummy
	@rm -f *~ \#*\#
	@rm -f $(OCAMLDOC) $(OCAMLDOC_OPT) *.cma *.cmxa *.cmo *.cmi *.cmx *.a *.o
	@rm -f odoc_parser.output odoc_text_parser.output
	@rm -f odoc_lexer.ml odoc_text_lexer.ml odoc_see_lexer.ml odoc_ocamlhtml.ml
	@rm -f odoc_parser.ml odoc_parser.mli odoc_text_parser.ml odoc_text_parser.mli odoc_crc.ml

.depend depend::
	rm -f .depend
	$(OCAMLYACC) odoc_text_parser.mly
	$(OCAMLYACC) odoc_parser.mly
	$(OCAMLLEX) odoc_text_lexer.mll
	$(OCAMLLEX) odoc_lexer.mll
	$(OCAMLDEP) $(INCLUDES) *.mll *.mly *.ml *.mli > .depend

dummy:

include .depend
