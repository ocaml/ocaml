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
char * caml_marshalled_global_data = NULL;
asize_t caml_marshalled_global_data_size = 0;
char * caml_section_table = NULL;
asize_t caml_section_table_size = 0;

enum caml_byte_program_mode caml_byte_program_mode = APPENDED;
