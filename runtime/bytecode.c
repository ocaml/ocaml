/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         David Allsopp, Tarides                         */
/*                                                                        */
/*   Copyright 2024 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/instruct.h"
#include "caml/startup.h"

opcode_t caml_start_code[] = {STOP};
asize_t caml_code_size = sizeof(caml_start_code);

enum caml_byte_program_mode caml_byte_program_mode = STANDARD;
