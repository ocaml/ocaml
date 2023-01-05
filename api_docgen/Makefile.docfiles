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

# Capitalize first letter of argument
define up
$(shell echo $(1) | cut -c1 | tr '[:lower:]' '[:upper:]')
endef

define capitalize_one
$(call up,$(1))$(shell echo $(1) | cut -c2-)
endef

define capitalize
$(foreach m,$(1),$(call capitalize_one,$m))
endef

runtime_events_MLIS := runtime_events.mli
str_MLIS := str.mli
unix_MLIS := unix.mli unixLabels.mli
dynlink_MLIS := dynlink.mli
thread_MLIS := \
  thread.mli event.mli

STDLIB=$(STDLIB_MODULES)

stdlib_UNPREFIXED=$(STDLIB_MODULE_BASENAMES)

otherlibref := $(dynlink_MLIS:%.mli=%)

ifeq "$(lib_str)" "true"
otherlibref += $(str_MLIS:%.mli=%)
endif

ifeq "$(lib_unix)" "true"
otherlibref += $(unix_MLIS:%.mli=%)
endif

ifeq "$(lib_systhreads)" "true"
otherlibref += $(thread_MLIS:%.mli=%)
endif

ifeq "$(lib_runtime_events)" "true"
otherlibref += $(runtime_events_MLIS:%.mli=%)
endif

libref_TEXT=Ocaml_operators Format_tutorial
libref_C=$(call capitalize,$(libref))

PARSING_MLIS := $(notdir $(wildcard $(ROOTDIR)/parsing/*.mli))
UTILS_MLIS := $(notdir $(wildcard $(ROOTDIR)/utils/*.mli))
DRIVER_MLIS := pparse.mli

compilerlibref_MLIS= \
  $(PARSING_MLIS) \
  $(UTILS_MLIS) \
  $(DRIVER_MLIS)
compilerlibref=$(compilerlibref_MLIS:%.mli=%)
compilerlibref_TEXT=Compiler_libs
compilerlibref_C=$(call capitalize,$(compilerlibref))

ALL_LIBREF= \
  $(sort $(libref_TEXT:%=libref/%)) \
  $(sort $(filter-out libref/camlinternal%, $(libref:%=libref/%))) \
  $(sort $(filter libref/camlinternal%, $(libref:%=libref/%)))

ALL_COMPILERLIBREF= \
  $(compilerlibref_TEXT:%=compilerlibref/%) \
  $(compilerlibref:%=compilerlibref/%)
# Note that the output of $(wildcard ...) is sorted alphabetically.
# The compilerlibs index will be thus be sorted first by category:
# - text documentation
# - parsing modules
# - utils modules
# - driver modules
# And then alphabetically inside each category.

ALL_DOC= $(ALL_LIBREF) $(ALL_COMPILERLIBREF)
