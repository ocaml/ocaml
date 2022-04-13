/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Signal handling, code specific to the native-code compiler */

#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/frame_descriptors.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/stack.h"

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to OCaml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection(void)
{
  frame_descr* d;
  intnat allocsz = 0;
  char *sp;
  uintnat retaddr;
  intnat whsize;

  caml_frame_descrs fds = caml_get_frame_descrs();
  struct stack_info* stack = Caml_state->current_stack;

  sp = (char*)stack->sp;
  Pop_frame_pointer(sp);
  retaddr = *(uintnat*)sp;

  /* Synchronise for the case when [young_limit] was used to interrupt
     us. */
  atomic_thread_fence(memory_order_acquire);

  { /* Find the frame descriptor for the current allocation */
    uintnat h = Hash_retaddr(retaddr, fds.mask);
    while (1) {
      d = fds.descriptors[h];
      if (d->retaddr == retaddr) break;
      h = (h + 1) & fds.mask;
    }
    /* Must be an allocation frame */
    CAMLassert(d && d->frame_size != 0xFFFF && (d->frame_size & 2));
  }

  { /* Compute the total allocation size at this point,
       including allocations combined by Comballoc */
    unsigned char* alloc_len = (unsigned char*)(&d->live_ofs[d->num_live]);
    int i, nallocs = *alloc_len++;

    if (nallocs == 0) {
      /* This is a poll */
      caml_process_pending_actions();
      return;
    }
    else
    {
      for (i = 0; i < nallocs; i++) {
        allocsz += Whsize_wosize(Wosize_encoded_alloc_len(alloc_len[i]));
      }
      /* We have computed whsize (including header)
         but need wosize (without) */
      allocsz -= 1;
    }

    whsize = Whsize_wosize(allocsz);

    /* Put the young pointer back to what is was before our triggering
       allocation */
    Caml_state->young_ptr += whsize;

    /* When caml_garbage_collection returns, we assume there is enough space in
      the minor heap for the triggering allocation. Due to finalisers in the
      major heap, it is possible for there to be a sequence of events where a
      single call to caml_handle_gc_interrupt does not lead to that. We do it
      in a loop to ensure it. */
    do {
      caml_process_pending_actions();
    } while
       ( (uintnat)(Caml_state->young_ptr - whsize) <=
         atomic_load_explicit(&Caml_state->young_limit, memory_order_acquire) );

    /* Re-do the allocation: we now have enough space in the minor heap. */
    Caml_state->young_ptr -= whsize;
  }
}
