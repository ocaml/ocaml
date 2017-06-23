BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
MAIN_MODULE=test
TEST_TEMP_FILES=link1 link2 test.txt

ifeq ($(OS),Windows_NT)
test:
	@$(MAKE) default
else
skip:
	@echo " ... testing => skipped (not on Windows)"
endif

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
