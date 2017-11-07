#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..

include $(BASEDIR)/../config/Makefile

default:
	@if $(BYTECODE_ONLY) || $(SKIP) ; then $(MAKE) skips ; else \
	  $(MAKE) all; \
	fi

all:
	@$(MAKE) tests

MLCASES=optargs staticalloc bind_tuples is_static register_typing \
  register_typing_switch
ARGS_optargs=-g
ARGS_is_static=-I $(OTOPDIR)/byterun is_in_static_data.c
MLCASES_FLAMBDA=is_static_flambda unrolling_flambda unrolling_flambda2
MLCASES_FLAMBDA_FLOAT=static_float_array_flambda \
                      static_float_array_flambda_opaque
ARGS_is_static_flambda=\
  -I $(OTOPDIR)/byterun is_in_static_data.c is_static_flambda_dep.ml
ARGS_static_float_array_flambda=\
  -I $(OTOPDIR)/byterun is_in_static_data.c simple_float_const.ml
ARGS_static_float_array_flambda_opaque=\
  -I $(OTOPDIR)/byterun is_in_static_data.c -opaque simple_float_const_opaque.ml
ARGS_staticalloc=-I $(OTOPDIR)/utils config.cmx

skips:
	@for c in $(MLCASES) $(MLCASES_FLAMBDA) $(MLCASES_FLAMBDA_FLOAT); do \
	  echo " ... testing '$$c': => skipped"; \
	done

one_ml:
	@$(OCAMLOPT) $(ARGS_$(NAME)) -o $(NAME).exe $(NAME).ml && \
	./$(NAME).exe && echo " => passed" || echo " => failed"

one_ml_cond:
	@if $(COND); then \
	   $(OCAMLOPT) $(ARGS_$(NAME)) -o $(NAME).exe $(NAME).ml && \
	   ./$(NAME).exe && echo " => passed" || echo " => failed"; \
	 else \
	   echo " => skipped"; \
	 fi

clean: defaultclean

include $(BASEDIR)/makefiles/Makefile.common

ifeq "$(WITH_SPACETIME)" "true"
# These tests have not been ported for Spacetime
SKIP=true
else
SKIP=false
endif

tests:
	@for c in $(MLCASES); do \
	  printf " ... testing '$$c':"; \
          $(MAKE) one_ml NAME=$$c; \
        done
	@for c in $(MLCASES_FLAMBDA); do \
	  printf " ... testing '$$c':"; \
          $(MAKE) one_ml_cond NAME=$$c COND=$(FLAMBDA); \
        done
	@for c in $(MLCASES_FLAMBDA_FLOAT); do \
	  printf " ... testing '$$c':"; \
	  $(MAKE) one_ml_cond NAME=$$c \
                  COND='$(FLAMBDA) && $(FLAT_FLOAT_ARRAY)'; \
        done

promote:

