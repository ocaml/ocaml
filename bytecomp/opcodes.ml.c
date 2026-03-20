/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2021 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include "caml/opcodes.h"

#define OPCODE_CONSTANT(name) \
let op ## name = opFIRST_UNIMPLEMENTED_OP \
let opFIRST_UNIMPLEMENTED_OP = op ## name + 1

let opFIRST_UNIMPLEMENTED_OP = 0
CAML_ZINC_OPCODES(OPCODE_CONSTANT)
