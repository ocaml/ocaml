/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 Gabriel Scherer, INRIA, Paris                          */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include "caml/opcodes.h"

#define OPCODE_DECL(name) \
val op ## name : int

CAML_ZINC_OPCODES(OPCODE_DECL)

val opFIRST_UNIMPLEMENTED_OP : int
