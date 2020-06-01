/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2006 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "frame_descriptors.h"
#include "caml/stack.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/fiber.h"
#include "caml/fail.h"

/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
static frame_descr * caml_next_frame_descriptor(caml_frame_descrs fds, uintnat * pc, char ** sp, struct stack_info* stack)
{
  frame_descr * d;

  while (1) {
    d = caml_find_frame_descr(fds, *pc);
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
      *sp += (d->frame_size & 0xFFFC);
      *pc = Saved_return_address(*sp);
      return d;
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous stack
       * chunk. This includes skipping over the DWARF link & trap frame (4 words). */
      *sp += 4 * sizeof(value);
      if (*sp == (char*)Stack_high(stack)) {
        /* We've reached the top of stack. No more frames. */
        *pc = 0;
        return NULL;
      }
      *sp += sizeof(struct caml_context) /* context */
          + sizeof(value); /* return address */
    }
  }
}

int caml_alloc_backtrace_buffer(void){
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
  if (Caml_state->backtrace_buffer == NULL) return -1;
  return 0;
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */
void caml_stash_backtrace(value exn, uintnat pc, char * sp, char* trapsp)
{
  caml_domain_state* domain_state = Caml_state;
  caml_frame_descrs fds;

  if (exn != caml_read_root(domain_state->backtrace_last_exn)) {
    domain_state->backtrace_pos = 0;
    caml_modify_root(domain_state->backtrace_last_exn, exn);
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  fds = caml_get_frame_descrs();
  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(fds, &pc, &sp, domain_state->current_stack);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (domain_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    domain_state->backtrace_buffer[domain_state->backtrace_pos++] =
      (backtrace_slot) descr;

    /* Stop when we reach the current exception handler */
    if (sp > trapsp) return;
  }
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */
static void get_callstack(struct stack_info* orig_stack, intnat max_frames,
                          frame_descr*** trace, intnat* trace_size)
{
  CAMLnoalloc;
  intnat trace_pos;
  char *sp;
  uintnat pc;
  caml_frame_descrs fds = caml_get_frame_descrs();

  /* first compute the size of the trace */
  {
    struct stack_info* stack = orig_stack;
    caml_get_stack_sp_pc(stack, &sp, &pc);
    trace_pos = 0;

    while(1) {
      frame_descr *descr = caml_next_frame_descriptor(fds, &pc, &sp, stack);
      if (trace_pos >= max_frames) break;
      if (descr == NULL) {
        stack = Stack_parent(stack);
        if (stack == NULL) break;
        caml_get_stack_sp_pc(stack, &sp, &pc);
      } else {
        ++trace_pos;
      }
    }
  }

  *trace_size = trace_pos;
  *trace = caml_stat_alloc(sizeof(frame_descr*) * trace_pos);

  /* then collect the trace */
  {
    struct stack_info* stack = orig_stack;
    caml_get_stack_sp_pc(stack, &sp, &pc);
    trace_pos = 0;

    while(1) {
      frame_descr *descr = caml_next_frame_descriptor(fds, &pc, &sp, stack);
      if (trace_pos >= max_frames) break;
      if (descr == NULL) {
        stack = Stack_parent(stack);
        if (stack == NULL) break;
        caml_get_stack_sp_pc(stack, &sp, &pc);
      } else {
        (*trace)[trace_pos] = descr;
        ++trace_pos;
      }
    }
  }
}

static value alloc_callstack(frame_descr** trace, intnat trace_len)
{
  CAMLparam0();
  CAMLlocal1(callstack);
  int i;
  callstack = caml_alloc(trace_len, 0);
  for (i = 0; i < trace_len; i++)
    Store_field(callstack, i, Val_backtrace_slot(trace[i]));
  caml_stat_free(trace);
  CAMLreturn(callstack);
}

CAMLprim value caml_get_current_callstack (value max_frames_value) {
  frame_descr** trace;
  intnat trace_len;
  get_callstack(Caml_state->current_stack, Long_val(max_frames_value),
                &trace, &trace_len);
  return alloc_callstack(trace, trace_len);
}

CAMLprim value caml_get_continuation_callstack (value cont, value max_frames)
{
  frame_descr** trace;
  intnat trace_len;
  struct stack_info* stack;

  stack = Ptr_val(caml_continuation_use(cont));
  {
    CAMLnoalloc;
    get_callstack(stack, max_frames,
                  &trace, &trace_len);
    caml_continuation_replace(cont, stack);
  }

  return alloc_callstack(trace, trace_len);
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  uintnat infoptr;
  frame_descr * d = (frame_descr *)slot;

  if ((d->frame_size & 1) == 0) {
    return NULL;
  }
  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  return *((debuginfo*)infoptr);
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  uint32_t * infoptr;

  if (dbg == NULL)
    return NULL;

  infoptr = dbg;
  infoptr += 2; /* Two packed info fields */
  return *((debuginfo*)infoptr);
}

/* Extract location information for the given frame descriptor */
void caml_debuginfo_location(debuginfo dbg, /*out*/ struct caml_loc_info * li)
{
  uint32_t info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if (dbg == NULL) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }
  /* Recover debugging info */
  info1 = ((uint32_t *)dbg)[0];
  info2 = ((uint32_t *)dbg)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call
                  1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to dbg
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) == 1;
  li->loc_is_inlined = caml_debuginfo_next(dbg) != NULL;
  li->loc_filename = (char *) dbg + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

CAMLprim value caml_add_debug_info(backtrace_slot start, value size,
                                   value events)
{
  return Val_unit;
}

CAMLprim value caml_remove_debug_info(backtrace_slot start)
{
  return Val_unit;
}

int caml_debug_info_available(void)
{
  return 1;
}
