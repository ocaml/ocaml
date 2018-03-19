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
