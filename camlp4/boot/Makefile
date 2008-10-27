

MAX_SAVE = 10

backup:
	cp camlp4boot camlp4boot.save.0
	set -e; for i in camlp4boot.save.*; do \
	  mv $$i camlp4boot.evas.$$((`echo $$i | sed -e 's/.*\.save\.\([0-9][0-9]*\)/\1/'` + 1)); \
	done
	set -e; for i in camlp4boot.evas.*; do mv $$i $${i/.evas./.save.}; done
	rm -f camlp4boot.save.$(MAX_SAVE)

restore:
	set -e; for i in camlp4boot.save.*; do \
	  mv $$i camlp4boot.evas.$$((`echo $$i | sed -e 's/.*\.save\.\([0-9][0-9]*\)/\1/'` - 1)); \
	done
	set -e; for i in camlp4boot.evas.*; do mv $$i $${i/.evas./.save.}; done
	mv camlp4boot.save.0 camlp4boot

boot-clean:
	rm -f camlp4boot.save.* camlp4boot.evas.*

maintainer-clean:
	rm -f camlp4boot
