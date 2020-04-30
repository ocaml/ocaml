/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/domain_state.h"
#include "caml/memory.h"

CAMLexport caml_domain_state* Caml_state;

void caml_init_domain ()
{
  if (Caml_state != NULL)
    return;

  Caml_state =
    (caml_domain_state*)caml_stat_alloc_noexc(sizeof(caml_domain_state));
  if (Caml_state == NULL)
    caml_fatal_error ("cannot initialize domain state");

  Caml_state->young_limit = NULL;
  Caml_state->exception_pointer = NULL;

  Caml_state->young_ptr = NULL;
  Caml_state->young_base = NULL;
  Caml_state->young_start = NULL;
  Caml_state->young_end = NULL;
  Caml_state->young_alloc_start = NULL;
  Caml_state->young_alloc_mid = NULL;
  Caml_state->young_alloc_end = NULL;
  Caml_state->young_trigger = NULL;
  Caml_state->minor_heap_wsz = 0;
  Caml_state->in_minor_collection = 0;
  Caml_state->extra_heap_resources_minor = 0;
  caml_alloc_minor_tables();

  Caml_state->stack_low = NULL;
  Caml_state->stack_high = NULL;
  Caml_state->stack_threshold = NULL;
  Caml_state->extern_sp = NULL;
  Caml_state->trapsp = NULL;
  Caml_state->trap_barrier = NULL;
  Caml_state->external_raise = NULL;
  Caml_state->exn_bucket = Val_unit;

  Caml_state->top_of_stack = NULL;
  Caml_state->bottom_of_stack = NULL; /* no stack initially */
  Caml_state->last_return_address = 1; /* not in OCaml code initially */
  Caml_state->gc_regs = NULL;

  Caml_state->stat_minor_words = 0.0;
  Caml_state->stat_promoted_words = 0.0;
  Caml_state->stat_major_words = 0.0;
  Caml_state->stat_minor_collections = 0;
  Caml_state->stat_major_collections = 0;
  Caml_state->stat_heap_wsz = 0;
  Caml_state->stat_top_heap_wsz = 0;
  Caml_state->stat_compactions = 0;
  Caml_state->stat_heap_chunks = 0;

  Caml_state->backtrace_active = 0;
  Caml_state->backtrace_pos = 0;
  Caml_state->backtrace_buffer = NULL;
  Caml_state->backtrace_last_exn = Val_unit;

  Caml_state->compare_unordered = 0;
  Caml_state->local_roots = NULL;
  Caml_state->requested_major_slice = 0;
  Caml_state->requested_minor_gc = 0;

  Caml_state->eventlog_enabled = 0;
  Caml_state->eventlog_paused = 0;
  Caml_state->eventlog_startup_pid = 0;
  Caml_state->eventlog_startup_timestamp = 0;
  Caml_state->eventlog_out = NULL;
}
