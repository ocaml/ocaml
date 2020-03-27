all: tools
	cd manual; ${MAKE} all
	${MAKE} tests
#	cd fpcl; ${MAKE} all

clean:
	cd manual; ${MAKE} clean
	cd tools; ${MAKE} clean
#	cd fpcl; ${MAKE} clean

release:
	cd manual; ${MAKE} release
#	cd fpcl; ${MAKE} release

.PHONY: tools
tools:
	cd tools; ${MAKE} clean; ${MAKE} all

# The pregen-etex target generates the latex files from the .etex
# files to ensure that this phase of the manual build process, which
# may execute OCaml fragments and expect certain outputs, is correct
pregen-etex: tools
	cd manual; $(MAKE) etex-files

# pregen builds both .etex files and the documentation of the standard library
pregen: tools
	cd manual; $(MAKE) files

# test the consistency of the manual and the compiler source
.PHONY:tests
tests:
	${MAKE} -C tests all
