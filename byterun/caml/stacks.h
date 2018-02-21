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

CAMLdata value * caml_stack_low;
CAMLdata value * caml_stack_high;
CAMLdata value * caml_stack_threshold;
CAMLdata value * caml_extern_sp;
CAMLdata value * caml_trapsp;
CAMLdata value * caml_trap_barrier;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void caml_init_stack (uintnat init_max_size);
void caml_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (uintnat new_max_size);
uintnat caml_stack_usage (void);

CAMLdata uintnat (*caml_stack_usage_hook)(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_STACKS_H */
