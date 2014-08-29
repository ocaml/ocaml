/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_ROOTS_H
#define CAML_ROOTS_H

#include "misc.h"
#include "memory.h"

typedef void (*scanning_action) (value, value *);

#ifndef NATIVE_CODE
struct caml_sampled_roots {
  value* stack_low;
  value* stack_high;
  struct caml__roots_block* local_roots;

  value* young_ptr;
  value* young_end;
  
  value* mark_stack;
  int mark_stack_count;

  struct addrmap* promotion_table;
  struct addrmap* promotion_rev_table;
  struct caml_heap_state* shared_heap;

  struct caml_runqueue* runqueue;
};
#else
#error "caml_sampled_roots not yet implemented for native code"
#endif

void caml_sample_local_roots(struct caml_sampled_roots*);
void caml_do_local_roots(scanning_action, struct caml_sampled_roots*);
void caml_do_sampled_roots(scanning_action, struct caml_sampled_roots*);


#endif /* CAML_ROOTS_H */
