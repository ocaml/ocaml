all: tools
	cd manual; ${MAKE} all
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

# The pregen target generates the latex files from the .etex files
# and the ocamldoc documentation for the standard library to ensure
# that at least this phase of the manual build process is correct
pregen: tools
	cd manual; ${MAKE} files
