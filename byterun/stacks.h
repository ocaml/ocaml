/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* structure of the stacks */

#ifndef _stacks_
#define _stacks_


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

extern value * stack_low;
extern value * stack_high;
extern value * stack_threshold;
extern value * extern_sp;
extern value * trapsp;
extern value * trap_barrier;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void reset_roots P((void));
void init_stack P((unsigned long init_max_size));
void realloc_stack P((void));
void change_max_stack_size P((unsigned long new_max_size));


#endif /* _stacks_ */

