all: tools manual tests


# The tools and the tests are rebuilt each time in order to avoid issues with
# different compiler versions
tests: manual
	$(MAKE) -C tests clean
	$(MAKE) -C tests all

tools:
	$(MAKE) -C tools clean
	$(MAKE) -C tools all
	$(MAKE) -C tests tools

manual: tools
	$(MAKE) -C src all

html: tools
	$(MAKE) -C src html

web: tools
	$(MAKE) -C src web

release:
	$(MAKE) -C src release

# The pregen-etex target generates the latex files from the .etex
# files to ensure that this phase of the manual build process, which
# may execute OCaml fragments and expect certain outputs, is correct
pregen-etex: tools
	$(MAKE) -C src etex-files

# pregen builds both .etex files and the documentation of the standard library
pregen: tools
	$(MAKE) -C src files


.PHONY: tests manual tools


.PHONY: clean
clean:
	$(MAKE) -C src clean
	$(MAKE) -C tools  clean
	$(MAKE) -C tests  clean

.PHONY: distclean
distclean: clean
	$(MAKE) -C src distclean
