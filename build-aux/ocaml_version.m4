#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sebastien Hinderer, projet Cambium, INRIA Paris             *
#*                                                                        *
#*   Copyright 2021 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# This file contains all the macros used to describe the current version of
# OCaml. It first defines the basic components and then computes all
# the different variants of the version used across the build system.

# For the M4 macros defined below, we use the OCAML__ (with a double
# underscore) to distinguish them from the C preprocessor macros which
# use a single underscore, since the two families of macros coexist
# in configure.ac.

# The following macro, OCAML__DEVELOPMENT_VERSION, should be either
# [true] of [false].

m4_define([OCAML__DEVELOPMENT_VERSION], [true])

# The three following components (major, minor and patch level) MUST be
# integers. They MUST NOT be left-padded with zeros and all of them,
# including the patchlevel, are mandatory.

m4_define([OCAML__VERSION_MAJOR], [5])
m4_define([OCAML__VERSION_MINOR], [4])
m4_define([OCAML__VERSION_PATCHLEVEL], [0])
# Note that the OCAML__VERSION_EXTRA string defined below is always empty
# for officially-released versions of OCaml.
m4_define([OCAML__VERSION_EXTRA], [dev0-2024-08-25])

# The OCAML__VERSION_EXTRA_PREFIX macro defined below should be a
# single character:
# Either [~] to mean that we are approaching the OCaml public release
# OCAML__VERSION_MAJOR.OCAML__VERSION_MINOR.OCAML__VERSION_PATCHLEVEL
# and with an empty OCAML__VERSION_EXTRA string;
# Or [+] to give more info about this specific version.
# Development releases, for instance, should use a [+] prefix.
m4_define([OCAML__VERSION_EXTRA_PREFIX], [+])
m4_define([OCAML__VERSION_SHORT], [OCAML__VERSION_MAJOR.OCAML__VERSION_MINOR])
# The OCAML__VERSION below must be in the format specified in stdlib/sys.mli
m4_define([OCAML__VERSION],
  [m4_do(
    OCAML__VERSION_SHORT.OCAML__VERSION_PATCHLEVEL,
    m4_if(OCAML__VERSION_EXTRA,[],[],
      OCAML__VERSION_EXTRA_PREFIX[]OCAML__VERSION_EXTRA))])

# Generate the VERSION file
# The following command is invoked when autoconf is run to generate configure
# from configure.ac, not while configure itself is run.
# In other words, both VERSION and configure are produced by invoking
# autoconf (usually done by calling tools/autogen for this project)
m4_syscmd([cat > VERSION << END_OF_VERSION_FILE
]OCAML__VERSION[

# Starting with OCaml 4.14, although the version string that appears above is
# still correct and this file can thus still be used to figure it out,
# the version itself is actually defined in the build-aux/ocaml_version.m4
# file (See the OCAML__VERSION* macros there.)
# To update the present VERSION file:
# 1. Update build-aux/ocaml_version.m4
# 2. Run tools/autogen.
# 3. If you are in a context where version control matters,
# commit the changes to both build-aux/ocaml_version.m4 and VERSION.
# The version string must be in the format described in stdlib/sys.mli
END_OF_VERSION_FILE
])

# Other variants of the version needed here and there in the compiler

m4_define([OCAML__VERSION_NUMBER],
  [m4_format(
    [%d%02d%02d],
    OCAML__VERSION_MAJOR,
    OCAML__VERSION_MINOR,
    OCAML__VERSION_PATCHLEVEL)])

m4_define([OCAML__RELEASE_EXTRA], 
  m4_if(OCAML__VERSION_EXTRA,[],[None],
      ['Some (]m4_if(OCAML__VERSION_EXTRA_PREFIX,+,[Plus],
      [Tilde])[, "]OCAML__VERSION_EXTRA[")']))]))

# Magic numbers for the different file formats

# The magic numbers are made of three components:
# - An 8-bytes prefix, common to all of them
# - A 1-byte kind, specifying the file type (exeecutable, cmi, cmo, etc.)
# - A 3-bytes version number

m4_define([MAGIC_NUMBER__PREFIX], [Caml1999])
m4_define([MAGIC_NUMBER__VERSION], [035])

# The following macro is used to define all our magic numbers
# Its first argument is the name of the file type described by that
# magic number and its second argument is the character used to
# characterize that file type

AC_DEFUN([DEFINE_MAGIC_NUMBER],
  [m4_define([$1__MAGIC_NUMBER],
    [MAGIC_NUMBER__PREFIX[]$2[]MAGIC_NUMBER__VERSION])])

m4_define(EXEC__FORMAT, [X])
DEFINE_MAGIC_NUMBER([EXEC], EXEC__FORMAT)
DEFINE_MAGIC_NUMBER([CMI], [I])
DEFINE_MAGIC_NUMBER([CMO], [O])
DEFINE_MAGIC_NUMBER([CMA], [A])
DEFINE_MAGIC_NUMBER([CMX_CLAMBDA], [Y])
DEFINE_MAGIC_NUMBER([CMX_FLAMBDA], [y])
DEFINE_MAGIC_NUMBER([CMXA_CLAMBDA], [Z])
DEFINE_MAGIC_NUMBER([CMXA_FLAMBDA], [z])
DEFINE_MAGIC_NUMBER([AST_IMPL], [M])
DEFINE_MAGIC_NUMBER([AST_INTF], [N])
DEFINE_MAGIC_NUMBER([CMXS], [D])
DEFINE_MAGIC_NUMBER([CMT], [T])
DEFINE_MAGIC_NUMBER([LINEAR], [L])
m4_define([MAGIC_NUMBER__LENGTH], m4_len(EXEC__MAGIC_NUMBER))
