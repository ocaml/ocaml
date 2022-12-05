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

#define CAML_INTERNALS

/* To initialize and resize the stacks */

#include <string.h>
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/stacks.h"

value caml_global_data = Val_unit; /* must be a valid value (#11768) */

uintnat caml_max_stack_size;            /* also used in gc_ctrl.c */

void caml_init_stack (uintnat initial_max_size)
{
  Caml_state->stack_low = (value *) caml_stat_alloc(Stack_size);
  Caml_state->stack_high = Caml_state->stack_low + Stack_size / sizeof (value);
  Caml_state->stack_threshold =
    Caml_state->stack_low + Stack_threshold / sizeof (value);
  Caml_state->extern_sp = Caml_state->stack_high;
  Caml_state->trapsp = Caml_state->stack_high;
  Caml_state->trap_barrier = Caml_state->stack_high + 1;
  caml_max_stack_size = initial_max_size;
  caml_gc_message (0x08, "Initial stack limit: %"
                   ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   caml_max_stack_size / 1024 * sizeof (value));
}

void caml_realloc_stack(asize_t required_space)
{
  asize_t size;
  value * new_low, * new_high, * new_sp;

  CAMLassert(Caml_state->extern_sp >= Caml_state->stack_low);
  size = Caml_state->stack_high - Caml_state->stack_low;
  do {
    if (size >= caml_max_stack_size) caml_raise_stack_overflow();
    size *= 2;
  } while (size < Caml_state->stack_high - Caml_state->extern_sp
                  + required_space);
  caml_gc_message (0x08, "Growing stack to %"
                         ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   (uintnat) size * sizeof(value) / 1024);
  new_low = (value *) caml_stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) Caml_state->stack_high - (char *) (ptr)))

  new_sp = (value *) shift(Caml_state->extern_sp);
  memmove((char *) new_sp,
          (char *) Caml_state->extern_sp,
          (Caml_state->stack_high - Caml_state->extern_sp) * sizeof(value));
  caml_stat_free(Caml_state->stack_low);
  Caml_state->trapsp = (value *) shift(Caml_state->trapsp);
  Caml_state->trap_barrier = (value *) shift(Caml_state->trap_barrier);
  Caml_state->stack_low = new_low;
  Caml_state->stack_high = new_high;
  Caml_state->stack_threshold =
    Caml_state->stack_low + Stack_threshold / sizeof (value);
  Caml_state->extern_sp = new_sp;

#undef shift
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->extern_sp - req < Caml_state->stack_low)
    caml_realloc_stack(req);
  return Val_unit;
}

void caml_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = Caml_state->stack_high - Caml_state->extern_sp
                 + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != caml_max_stack_size){
    caml_gc_message (0x08, "Changing stack limit to %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                     new_max_size * sizeof (value) / 1024);
  }
  caml_max_stack_size = new_max_size;
}

CAMLexport uintnat (*caml_stack_usage_hook)(void) = NULL;

uintnat caml_stack_usage(void)
{
  uintnat sz;
  sz = Caml_state->stack_high - Caml_state->extern_sp;
  if (caml_stack_usage_hook != NULL)
    sz += (*caml_stack_usage_hook)();
  return sz;
}
