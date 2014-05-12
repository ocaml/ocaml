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

/* To initialize and resize the stacks */

#include <string.h>
#include "config.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"
#include "startup.h"

CAMLexport __thread value * caml_stack_low; /* allocation base */
CAMLexport __thread value * caml_stack_high; /* one-past-the-end */
CAMLexport __thread value * caml_stack_threshold; /* low + Stack_threshold */
CAMLexport __thread value * caml_extern_sp;
CAMLexport __thread intnat caml_trap_sp_off;
CAMLexport __thread intnat caml_trap_barrier_off;
caml_root caml_global_data;

uintnat caml_max_stack_size;            /* also used in gc_ctrl.c */

void caml_init_stack ()
{
  caml_stack_low = (value *) caml_stat_alloc(Stack_size);
  caml_stack_high = caml_stack_low + Stack_size / sizeof (value);
  caml_stack_threshold = caml_stack_low + Stack_threshold / sizeof (value);
  caml_extern_sp = caml_stack_high;
  caml_trap_sp_off = 0;
  caml_trap_barrier_off = 1;
  caml_max_stack_size = caml_startup_params.max_stack_init;
  caml_gc_log ("Initial stack limit: %luk bytes",
               caml_max_stack_size / 1024 * sizeof (value));
}

void caml_realloc_stack(asize_t required_space)
{
  asize_t size;
  value * new_low, * new_high, * new_sp;

  Assert(caml_extern_sp >= caml_stack_low);
  size = caml_stack_high - caml_stack_low;
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < caml_stack_high - caml_extern_sp + required_space);
  caml_gc_log ("Growing stack to %"
                         ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   (uintnat) size * sizeof(value) / 1024);
  new_low = (value *) caml_stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) caml_stack_high - (char *) (ptr)))

  new_sp = (value *) shift(caml_extern_sp);
  memmove((char *) new_sp,
          (char *) caml_extern_sp,
          (caml_stack_high - caml_extern_sp) * sizeof(value));
  caml_stat_free(caml_stack_low);
  caml_stack_low = new_low;
  caml_stack_high = new_high;
  caml_stack_threshold = caml_stack_low + Stack_threshold / sizeof (value);
  caml_extern_sp = new_sp;

#undef shift
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (caml_extern_sp - req < caml_stack_low) caml_realloc_stack(req);
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = caml_stack_high - caml_extern_sp
                 + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != caml_max_stack_size){
    caml_gc_log ("Changing stack limit to %luk bytes",
                     new_max_size * sizeof (value) / 1024);
  }
  caml_max_stack_size = new_max_size;
}

CAMLexport uintnat (*caml_stack_usage_hook)(void) = NULL;

uintnat caml_stack_usage(void)
{
  uintnat sz;
  sz = caml_stack_high - caml_extern_sp;
  if (caml_stack_usage_hook != NULL)
    sz += (*caml_stack_usage_hook)();
  return sz;
}
