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

/* structure of the stacks */

#ifndef CAML_STACKS_H
#define CAML_STACKS_H

#ifdef CAML_INTERNALS

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

/* Global variables moved to Caml_state in 4.10 */
#define caml_stack_low (Caml_state_field(stack_low))
#define caml_stack_high (Caml_state_field(stack_high))
#define caml_stack_threshold (Caml_state_field(stack_threshold))
#define caml_extern_sp (Caml_state_field(extern_sp))
#define caml_trapsp (Caml_state_field(trapsp))
#define caml_trap_barrier (Caml_state_field(trap_barrier))

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link_offset(tp) (((value *)(tp))[1])

void caml_init_stack (uintnat init_max_size);
void caml_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (uintnat new_max_size);
uintnat caml_stack_usage (void);

CAMLextern uintnat (*caml_stack_usage_hook)(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_STACKS_H */
