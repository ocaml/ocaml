/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* structure of the stacks */

#ifndef CAML_STACKS_H
#define CAML_STACKS_H


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

CAMLextern value * caml_stack_low;
CAMLextern value * caml_stack_high;
CAMLextern value * caml_stack_threshold;
CAMLextern value * caml_extern_sp;
CAMLextern value * caml_trapsp;
CAMLextern value * caml_trap_barrier;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void caml_init_stack (unsigned long init_max_size);
void caml_realloc_stack (asize_t required_size);
void caml_change_max_stack_size (unsigned long new_max_size);


#endif /* CAML_STACKS_H */
