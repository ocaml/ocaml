/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* To initialize and resize the stacks */

#include <string.h>
#include "config.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

value * stack_low;
value * stack_high;
value * stack_threshold;
value * extern_sp;
value * trapsp;
value global_data;

void init_stack()
{
  stack_low = (value *) stat_alloc(Stack_size);
  stack_high = stack_low + Stack_size / sizeof (value);
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = stack_high;
  trapsp = stack_high;
}

void realloc_stack()
{        
  asize_t size;
  value * new_low, * new_high, * new_sp;
  value * p;

  Assert(extern_sp >= stack_low);
  size = stack_high - stack_low;
  if (size >= Max_stack_size)
    raise_out_of_memory();
  size *= 2;
  gc_message ("Growing stack to %ld kB.\n",
	      (long) size * sizeof(value) / 1024);
  new_low = (value *) stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) stack_high - (char *) (ptr)))

  new_sp = (value *) shift(extern_sp);
  bcopy((char *) extern_sp,
        (char *) new_sp,
        (stack_high - extern_sp) * sizeof(value));
  stat_free((char *) stack_low);
  trapsp = (value *) shift(trapsp);
  for (p = trapsp; p < new_high; p = Trap_link(p))
    Trap_link(p) = (value *) shift(Trap_link(p));
  stack_low = new_low;
  stack_high = new_high;
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = new_sp;

#undef shift
}

