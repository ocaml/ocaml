#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*             Florian Angeletti, projet Cambium, Inria Paris             *
#*                                                                        *
#*   Copyright 2020 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************
ROOTDIR = ..
-include $(ROOTDIR)/Makefile.config

ifeq ($(DOCUMENTATION_TOOL),odoc)
  include odoc/Makefile
else
  include ocamldoc/Makefile
endif

odoc-%:
	$(MAKE) -C odoc $* ROOTDIR=../..

ocamldoc-%:
	$(MAKE) -C ocamldoc $* ROOTDIR=../..

clean:
	rm -rf build odoc/build ocamldoc/build
