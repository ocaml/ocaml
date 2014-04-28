/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* structure of the stacks */

#ifndef CAML_STACKS_H
#define CAML_STACKS_H


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

CAMLextern __thread value * caml_stack_low;
CAMLextern __thread value * caml_stack_high;
CAMLextern __thread value * caml_stack_threshold;
CAMLextern __thread value * caml_extern_sp;
CAMLextern __thread intnat caml_trap_sp_off;
CAMLextern __thread intnat caml_trap_barrier_off;

/* The table of global identifiers */

extern caml_root caml_global_data;


#define Val_off(off) (caml_stack_high + Long_val (off))
#define Off_val(p) Val_long (caml_stack_high - Long_val (p))
#define Trap_pc(tp) Field((tp), 0)
#define Trap_link(tp) Field((tp), 1)

void caml_init_stack (uintnat init_max_size);
void caml_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (uintnat new_max_size);
uintnat caml_stack_usage (void);

CAMLextern uintnat (*caml_stack_usage_hook)(void);

#endif /* CAML_STACKS_H */
