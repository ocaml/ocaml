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
#include "caml/frame_descriptors.h"
#include "caml/stack.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/fiber.h"
#include "caml/fail.h"

/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
static frame_descr *next_frame_descriptor
    (caml_frame_descrs fds, uintnat *pc, char **sp, struct stack_info *stack)
{
  frame_descr *d;

  while (true) {
    d = caml_find_frame_descr(fds, *pc);

    if (!d) {
      return NULL;
    }

    /* Skip to next frame */
    if (!frame_return_to_C(d)) {
      /* Regular frame, update sp/pc and return the frame descriptor */
      *sp += frame_size(d);
      *pc = Saved_return_address(*sp);
      return d;
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous stack
       chunk. This includes skipping over the DWARF link & trap frame
       (4 words). */
      *sp += Stack_header_size;
      if (*sp == (char*)Stack_high(stack)) {
        /* We've reached the top of stack. No more frames. */
        *pc = 0;
        return NULL;
      }
      *sp = First_frame(*sp);
      *pc = Saved_return_address(*sp);
    }
  }
}

int caml_alloc_backtrace_buffer(void)
{
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
  if (Caml_state->backtrace_buffer == NULL) {
    return -1;
  }
  return 0;
}

void caml_free_backtrace_buffer(backtrace_slot *backtrace_buffer) {
  if (backtrace_buffer != NULL)
    caml_stat_free(backtrace_buffer);
}

/* initial size of a callstack buffer, in entries */

#define MIN_CALLSTACK_SIZE 16

/* Stores upto [max_frames] frames of the current call stack. Stop when
   we reach that frame count, or when the SP exceeds [trap_sp], if given.
*/
static size_t get_callstack(struct stack_info *stack, char *sp, uintnat pc,
                            char *trap_sp, size_t max_frames,
                            frame_descr*** trace_p, size_t *allocated_size_p)
{
  caml_frame_descrs fds;
  size_t size = *allocated_size_p;
  frame_descr **trace = *trace_p;
  size_t frames = 0;

  CAMLnoalloc;

  fds = caml_get_frame_descrs();

  while(frames < max_frames) {
    frame_descr *descr = next_frame_descriptor(fds, &pc, &sp, stack);
    if (!descr) { /* end of current stack fragment */
      stack = Stack_parent(stack);
      if (stack == NULL) break;
      caml_get_stack_sp_pc(stack, &sp, &pc);
    } else {
      if (frames == size) {
        size_t new_size = size ? size * 2 : MIN_CALLSTACK_SIZE;
        trace = caml_stat_resize_noexc(trace, sizeof(frame_descr*) * new_size);
        if (!trace) {
          *trace_p = NULL;
          *allocated_size_p = 0;
          return 0;
        }
        size = new_size;
      }
      trace[frames] = descr;
      ++frames;
      if (trap_sp && sp > trap_sp)
        break;
    }
  }

  *trace_p = trace;
  *allocated_size_p = size;
  return frames;
}

/* Get the current backtrace for an exception, into a per-domain
   statically-sized buffer. */

void caml_stash_backtrace(value exn, uintnat pc, char * sp, char* trapsp)
{
  caml_domain_state* domain_state = Caml_state;

  if (exn != domain_state->backtrace_last_exn) {
    domain_state->backtrace_pos = 0;
    caml_modify_generational_global_root
      (&domain_state->backtrace_last_exn, exn);
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  size_t allocated_size = 0;
  frame_descr **backtrace_buffer =
          (frame_descr**)domain_state->backtrace_buffer;
  domain_state->backtrace_pos = get_callstack(domain_state->current_stack,
                                              sp, pc, trapsp,
                                              BACKTRACE_BUFFER_SIZE,
                                              &backtrace_buffer,
                                              &allocated_size);
  domain_state->backtrace_buffer = (void**)backtrace_buffer;
  CAMLassert(domain_state->backtrace_pos <= BACKTRACE_BUFFER_SIZE);
  CAMLassert(allocated_size == BACKTRACE_BUFFER_SIZE);
}

/* Get the current backtrace from the current stack_info. */

static size_t user_get_callstack(struct stack_info *stack,
                                 size_t max_frames,
                                 frame_descr ***trace_p,
                                 size_t *allocated_size_p)
{
  char *sp;
  uintnat pc;

  caml_get_stack_sp_pc(stack, &sp, &pc);
  return get_callstack(stack, sp, pc, NULL,
                       max_frames, trace_p, allocated_size_p);
}

/* Get the current raw callstack into a caller-provided buffer. */

size_t caml_get_callstack (size_t max_frames, void **buffer, size_t *alloc_size)
{
  frame_descr **buf = (frame_descr**)*buffer;
  size_t alloc_frames = (*alloc_size) / sizeof(frame_descr *);
  size_t frames = user_get_callstack(Caml_state->current_stack, max_frames,
                                     &buf, &alloc_frames);
  *buffer = buf;
  *alloc_size = alloc_frames * sizeof(frame_descr *);
  return frames;
}

static value alloc_callstack(frame_descr** trace, size_t frames)
{
  CAMLparam0();
  CAMLlocal1(callstack);
  int i;
  callstack = caml_alloc(frames, 0);
  for (i = 0; i < frames; i++)
    Store_field(callstack, i, Val_backtrace_slot(trace[i]));
  caml_stat_free(trace);
  CAMLreturn(callstack);
}

CAMLprim value caml_get_current_callstack (value max_frames_value)
{
  frame_descr** trace = NULL;
  size_t alloc_size = 0;
  size_t frames = user_get_callstack(Caml_state->current_stack,
                                     Long_val(max_frames_value),
                                     &trace, &alloc_size);
  return alloc_callstack(trace, frames);
}

CAMLprim value caml_get_continuation_callstack(value cont,
                                               value max_frames_value)
{
  size_t frames;
  frame_descr** trace = NULL;
  size_t alloc_size = 0;
  struct stack_info* stack;

  stack = Ptr_val(caml_continuation_use(cont));
  {
    CAMLnoalloc;
    frames = user_get_callstack(stack, Long_val(max_frames_value),
                                &trace, &alloc_size);
    caml_continuation_replace(cont, stack);
  }

  return alloc_callstack(trace, frames);
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  unsigned char* infoptr;
  uint32_t debuginfo_offset;
  frame_descr * d = (frame_descr *)slot;

  /* The special frames marking returns from Caml to C are never
     returned by next_frame_descriptor, so should never reach
     here. */
  CAMLassert(!frame_return_to_C(d));

  if (!frame_has_debug(d)) {
    return NULL;
  }
  /* Recover debugging info */
  infoptr = (unsigned char*)&d->live_ofs[d->num_live];
  if (frame_has_allocs(d)) {
    /* skip alloc_lengths */
    infoptr += *infoptr + 1;
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    /* we know there's at least one valid debuginfo,
       but it may not be the one for the first alloc */
    while (*(uint32_t*)infoptr == 0) {
      infoptr += sizeof(uint32_t);
    }
  } else {
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
  }
  /* read offset to debuginfo */
  debuginfo_offset = *(uint32_t*)infoptr;
  return (debuginfo)(infoptr + debuginfo_offset);
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  uint32_t * infoptr;

  if (dbg == NULL)
    return NULL;

  infoptr = dbg;
  if ((infoptr[0] & 1) == 0)
    /* No next debuginfo */
    return NULL;
  else
    /* Next debuginfo is after the two packed info fields */
    return (debuginfo*)(infoptr + 2);
}

/* Multiple names may share the same filename,
   so it is referenced as an offset instead of stored inline */
struct name_info {
  int32_t filename_offs;
  char name[1];
};

/* Extract location information for the given frame descriptor */
void caml_debuginfo_location(debuginfo dbg, /*out*/ struct caml_loc_info * li)
{
  uint32_t info1, info2;
  struct name_info * name_info;

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
  name_info = (struct name_info*)((char *) dbg + (info1 & 0x3FFFFFC));
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb ffffffffffffffffffffffff k n
                         44       36         26                        2 1 0
                       (32+12)    (32+4)
     n ( 1 bit ): 0 if this is the final debuginfo
                  1 if there's another following this one
     k ( 1 bit ): 0 if it's a call
                  1 if it's a raise
     f (24 bits): offset (in 4-byte words) of file name relative to dbg
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 2) == 2;
  li->loc_is_inlined = caml_debuginfo_next(dbg) != NULL;
  li->loc_defname = name_info->name;
  li->loc_filename =
    (char *)name_info + name_info->filename_offs;
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

value caml_add_debug_info(backtrace_slot start, value size, value events)
{
  return Val_unit;
}

value caml_remove_debug_info(backtrace_slot start)
{
  return Val_unit;
}

int caml_debug_info_available(void)
{
  return 1;
}

int caml_debug_info_status(void)
{
  return 1;
}
