# $Id$

DIRS=dynlink

all:
	set -e; \
	for i in $(DIRS); do cd $$i; $(MAKE) all EXE=$(EXE); cd ..; done

clean:
	set -e; \
	for i in $(DIRS); do cd $$i; $(MAKE) clean EXE=$(EXE); cd ..; done

depend:
	set -e; for i in $(DIRS); do cd $$i; $(MAKE) depend; cd ..; done
