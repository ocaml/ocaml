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
CSC=csc

COMPFLAGS=-I $(OTOPDIR)/otherlibs/bigarray -I $(OTOPDIR)/otherlibs/dynlink \
          -I $(OTOPDIR)/byterun
LD_PATH=$(TOPDIR)/otherlibs/bigarray:$(TOPDIR)/otherlibs/dynlink

default:
	@$(SET_LD_PATH) $(MAKE) all

.PHONY: all
all: prepare bytecode bytecode-dll native native-dll

.PHONY: prepare
prepare:
	@if $(SUPPORTS_SHARED_LIBRARIES); then \
	   $(OCAMLC) -c plugin.ml && \
	   if $(BYTECODE_ONLY) ; then : ; else \
	     $(OCAMLOPT) -o plugin.cmxs -shared plugin.ml; \
	   fi; \
	 fi

.PHONY: bytecode
bytecode:
	@printf " ... testing 'bytecode':"
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || ! which $(CSC) >/dev/null 2>&1; \
	then \
	  echo " => skipped"; \
	else \
	  $(OCAMLC) -output-obj -o main.dll dynlink.cma main.ml entry.c; \
	  $(CSC) /nologo /nowarn:1668 /out:main.exe main.cs; \
	  ./main.exe > bytecode.result; \
	  $(DIFF) bytecode.reference bytecode.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: bytecode-dll
bytecode-dll:
	@printf " ... testing 'bytecode-dll':"
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || ! which $(CSC) > /dev/null 2>&1; \
	then \
	  echo " => skipped"; \
	else \
	  $(OCAMLC) -output-obj -o main_obj.$(O) dynlink.cma entry.c main.ml; \
	  $(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) \
	           $(CTOPDIR)/byterun/libcamlrun.$(A)  $(BYTECCLIBS); \
	  $(CSC) /nologo /nowarn:1668 /out:main.exe main.cs; \
	  ./main.exe >bytecode-dll.result; \
	  $(DIFF) bytecode.reference bytecode-dll.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: native
native:
	@printf " ... testing 'native':"
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || $(BYTECODE_ONLY) \
	    || ! which $(CSC) > /dev/null 2>&1; then \
	  echo " => skipped"; \
	else \
	  $(OCAMLOPT) -output-obj -o main.dll dynlink.cmxa entry.c main.ml; \
	  $(CSC) /nologo /nowarn:1668 /out:main.exe main.cs; \
	  ./main.exe > native.result; \
	  $(DIFF) native.reference native.result > /dev/null \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: native-dll
native-dll:
	@printf " ... testing 'native-dll':"
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || $(BYTECODE_ONLY) \
	    || ! which $(CSC) > /dev/null 2>&1; then \
	  echo " => skipped"; \
	else \
	  $(OCAMLOPT) -output-obj -o main_obj.$(O) dynlink.cmxa entry.c \
	              main.ml; \
	  $(MKDLL) -maindll -o main.dll main_obj.$(O) entry.$(O) \
	           $(CTOPDIR)/asmrun/libasmrun.lib -v; \
	  $(CSC) /nologo /nowarn:1668 /out:main.exe main.cs; \
	  ./main.exe > native-dll.result; \
	  $(DIFF) native.reference native-dll.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.exe *.dll *.so *.obj *.o

include $(BASEDIR)/makefiles/Makefile.common
