/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The instruction set. */

#ifndef CAML_INSTRUCT_H
#define CAML_INSTRUCT_H

#ifdef CAML_INTERNALS

#include "opcodes.h"

#define OPCODE_ENTRY(name) name,
enum instructions {
  CAML_ZINC_OPCODES(OPCODE_ENTRY)
  FIRST_UNIMPLEMENTED_OP
};
#undef OPCODE_ENTRY

#endif /* CAML_INTERNALS */

#endif /* CAML_INSTRUCT_H */
