BASEDIR=../..

LD_PATH=

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || $(BYTECODE_ONLY) ; then \
	  echo 'skipped (.cmxs not built)'; \
	elif ! grep -q HAS_LIBBFD $(TOPDIR)/byterun/caml/s.h ; then \
	  echo 'skipped (BFD library not available)'; \
	else \
	  case " $(OTHERLIBRARIES) " in \
	    *' unix '* | *' win32unix '*) \
	       $(SET_LD_PATH) OCAMLLIB=$(TOPDIR)/tools $(MAKE) run;; \
	    *) echo 'skipped (unix.cmxs not available)';; \
	  esac \
	fi

.PHONY: run
run:
	@printf " ... testing 'ocamlobjinfo'"
	@rm -f $(MAIN_MODULE).result
	@$(OCAMLRUN) `$(CYGPATH) $(TOPDIR)/tools/ocamlobjinfo$(EXE)` \
	             $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix/unix.cmxs \
	             > test.raw.result 2>&1 \
	 && sed -e 's/\([^0-9a-z]\)[0-9a-z]\{32\}\([^0-9a-z]\|$$\)/\1<MD5>\2/' \
	        -e 's/File .*otherlibs/File otherlibs/' \
	             test.raw.result > test.result \
	 && $(DIFF) test.$(BACKEND).reference test.result > /dev/null \
	 && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common

ifeq "$(FLAMBDA)" "true"
BACKEND=flambda
else
BACKEND=clambda
endif
