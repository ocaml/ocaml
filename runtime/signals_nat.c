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
  caml_domain_state * dom_st = Caml_state;
  caml_frame_descrs fds = caml_get_frame_descrs();
  struct stack_info* stack = dom_st->current_stack;

  char * sp = (char*)stack->sp;
  sp = First_frame(sp);
  uintnat retaddr = Saved_return_address(sp);

  /* Synchronise for the case when [young_limit] was used to interrupt
     us. */
  atomic_thread_fence(memory_order_acquire);

  { /* Find the frame descriptor for the current allocation */
    d = caml_find_frame_descr(fds, retaddr);
    /* Must be an allocation frame */
    CAMLassert(d && !frame_return_to_C(d) && frame_has_allocs(d));
  }

  { /* Compute the total allocation size at this point,
       including allocations combined by Comballoc */
    unsigned char* alloc_len = (unsigned char*)(&d->live_ofs[d->num_live]);
    int i, nallocs = *alloc_len++;
    intnat allocsz = 0;

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

    caml_alloc_small_dispatch(dom_st, allocsz, CAML_DO_TRACK | CAML_FROM_CAML,
                              nallocs, alloc_len);
  }
}

/* Trap handling for the POWER architecture.  Convert the trap into
   an out-of-bounds exception. */

#if defined(TARGET_power)

extern void caml_array_bound_error_asm(void);

void caml_sigtrap_handler(int signo, siginfo_t * info, void * context)
{
  /* The trap occurs in ocamlopt-generated code. */
  /* The purpose of this function is to simulate a [caml_c_call]
     to [caml_array_bound_error_asm]. */

  caml_domain_state * dom_st = Caml_state;
  /* Recover the values of interesting registers. */
  ucontext_t * ctx = context;
  uint64_t ctx_pc = ctx->uc_mcontext.gp_regs[32];
  uint64_t ctx_sp = ctx->uc_mcontext.gp_regs[1];
  uint64_t ctx_exn_ptr = ctx->uc_mcontext.gp_regs[29];
  uint64_t ctx_young_ptr = ctx->uc_mcontext.gp_regs[31];
  /* Save address of trap as the return address in the standard stack frame
     location, so that it will be recorded in the stack backtrace. */
  ((uint64_t *) ctx_sp)[2] = ctx_pc;
  /* Record the OCaml stack pointer (for backtraces) */
  /* Update the exception handler pointer and the allocation pointer */
  dom_st->current_stack-> sp = (void *) ctx_sp;
  dom_st->young_ptr = (value *) ctx_young_ptr;
  dom_st->exn_handler = (void *) ctx_exn_ptr;
  /* Raise the exception */
  caml_array_bound_error_asm();
}

#endif
