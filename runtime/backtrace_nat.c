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

/* A backtrace_slot is either a debuginfo or a frame_descr* */
#define Slot_is_debuginfo(s) ((uintnat)(s) & 2)
#define Debuginfo_slot(s) ((debuginfo)((uintnat)(s) - 2))
#define Slot_debuginfo(d) ((backtrace_slot)((uintnat)(d) + 2))
#define Frame_descr_slot(s) ((frame_descr*)(s))
#define Slot_frame_descr(f) ((backtrace_slot)(f))

static debuginfo debuginfo_extract(frame_descr *d, ssize_t alloc_idx);

/* initial size of a callstack buffer, in entries */

#define MIN_CALLSTACK_SIZE 16

/* Stores up to [max_frames] frame descriptors of a stack in a buffer
   [*trace_p] of length [*allocated_size_p], allocated in the C
   heap. Resize the buffer as required. Start at [sp]/[pc] on [stack],
   and at offfset [frames] in the buffer. Stop when we reach
   [max_frames], or at the top of this fiber (unless [with_parents] is
   set, or when the stack pointer exceeds [trap_sp], if given.
 */
static size_t get_callstack(struct stack_info *stack, char *sp, uintnat pc,
                            char *trap_sp, bool with_parents, ssize_t alloc_idx,
                            size_t max_frames, size_t frames,
                            backtrace_slot** trace_p, size_t *allocated_size_p)
{
  caml_frame_descrs fds;
  size_t size = *allocated_size_p;
  backtrace_slot *trace = *trace_p;

  CAMLnoalloc;

  fds = caml_get_frame_descrs();

  while(frames < max_frames) {
    frame_descr *descr = next_frame_descriptor(fds, &pc, &sp, stack);
    if (!descr) { /* end of current stack fragment */
      stack = Stack_parent(stack);
      if (stack == NULL || !with_parents) break;
      caml_get_stack_sp_pc(stack, &sp, &pc);
    } else {
      backtrace_slot slot = Slot_frame_descr(descr);
      if (alloc_idx >= 0) {
        debuginfo info = debuginfo_extract(descr, alloc_idx);
        if (info) {
          CAMLassert(((uintnat)info & 3) == 0); /* so we can tag it */
          slot = Slot_debuginfo(info);
        }
        alloc_idx = -1;
      }
      if (frames == size) {
        size_t new_size = size ? size * 2 : MIN_CALLSTACK_SIZE;
        trace = caml_stat_resize_noexc(trace,
                                       sizeof(backtrace_slot) * new_size);
        if (!trace) {
          *trace_p = NULL;
          *allocated_size_p = 0;
          return 0;
        }
        size = new_size;
      }
      trace[frames] = slot;
      ++frames;
      if (trap_sp && (sp > trap_sp))
        break;
    }
  }

  *trace_p = trace;
  *allocated_size_p = size;
  return frames;
}

/* Get the current backtrace for an exception, into a per-domain
   buffer. If the exception is the same as the last exception, add
   frames to the existing buffer (thus allowing re-raise stack regions
   to be appended). */

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

  size_t allocated_size = BACKTRACE_BUFFER_SIZE;
  backtrace_slot *backtrace_buffer = domain_state->backtrace_buffer;
  domain_state->backtrace_pos = get_callstack(domain_state->current_stack,
                                              sp, pc, trapsp,
                                              false, -1,
                                              BACKTRACE_BUFFER_SIZE,
                                              domain_state->backtrace_pos,
                                              &backtrace_buffer,
                                              &allocated_size);
  /* Because domain_state->backtrace_buffer is originally allocated with
   * size BACKTRACE_BUFFER_SIZE, and we ask for no more than that many frames,
   * get_callstack() should never resize it. */
  CAMLassert(allocated_size == BACKTRACE_BUFFER_SIZE);
  CAMLassert(backtrace_buffer == domain_state->backtrace_buffer);
  CAMLassert(domain_state->backtrace_pos <= BACKTRACE_BUFFER_SIZE);

  domain_state->backtrace_buffer = backtrace_buffer;
}

/* Stores up to [max_frames] frame descriptors of [stack] in a buffer
   [*trace_p] of length [*allocated_size_p], allocated in the C
   heap. Resize the buffer as required. Include parent fibers.
 */

static size_t user_get_callstack(struct stack_info *stack,
                                 ssize_t alloc_idx,
                                 size_t max_frames,
                                 backtrace_slot **trace_p,
                                 size_t *allocated_size_p)
{
  char *sp;
  uintnat pc;

  caml_get_stack_sp_pc(stack, &sp, &pc);
  return get_callstack(stack, sp, pc, NULL, true, alloc_idx,
                       max_frames, 0, trace_p, allocated_size_p);
}

/* Stores up to [max_frames] frame descriptors of the current call
   stack, in a buffer [*buffer] of size [*alloc_size_p] bytes,
   allocated in the C heap. Resize the buffer as required. Include
   parent fibers. Returns the number of frames.
 */

size_t caml_get_callstack(size_t max_frames,
                          backtrace_slot **buffer_p,
                          size_t *alloc_size_p,
                          ssize_t alloc_idx)
{
  return user_get_callstack(Caml_state->current_stack, alloc_idx,
                            max_frames, buffer_p, alloc_size_p);
}

/* Create and return a Caml [Printexc.raw_backtrace] (an array of
 * [raw_backtrace_entry] values), from [frames] entries of [trace].
 */

static value alloc_callstack(backtrace_slot *trace, size_t frames)
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

/* Create and return a [Printexc.raw_backtrace] of the current
 * callstack, of up to [max_frames_value] entries. Includes parent
 * fibers.
 */

CAMLprim value caml_get_current_callstack(value max_frames_value)
{
  backtrace_slot *trace = NULL;
  size_t alloc_size = 0;
  size_t frames = user_get_callstack(Caml_state->current_stack, -1,
                                     Long_val(max_frames_value),
                                     &trace, &alloc_size);
  return alloc_callstack(trace, frames);
}

/* Create and return a [Printexc.raw_backtrace] of the callstack of
 * the continuation [cont], of up to [max_frames_value]
 * entries. Includes parent fibers.
 */

CAMLprim value caml_get_continuation_callstack(value cont,
                                               value max_frames_value)
{
  size_t frames;
  backtrace_slot *trace = NULL;
  size_t alloc_size = 0;
  struct stack_info* stack;

  stack = Ptr_val(caml_continuation_use(cont));
  {
    CAMLnoalloc;
    frames = user_get_callstack(stack, -1,
                                Long_val(max_frames_value),
                                &trace, &alloc_size);
    caml_continuation_replace(cont, stack);
  }

  return alloc_callstack(trace, frames);
}

static debuginfo debuginfo_extract(frame_descr *d, ssize_t alloc_idx)
{
  unsigned char* infoptr;
  uint32_t debuginfo_offset;

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
    /* find debug info for this allocation */
    if (alloc_idx >= -1) {
      infoptr += alloc_idx * sizeof(uint32_t);
    } else {
      /* Any allocation will do: look for a valid debuginfo */
      while (*(uint32_t*)infoptr == 0) {
        infoptr += sizeof(uint32_t);
      }
    }
  } else {
    CAMLassert(alloc_idx == -1);
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
  }
  /* read offset to debuginfo */
  debuginfo_offset = *(uint32_t*)infoptr;
  return (debuginfo)(infoptr + debuginfo_offset);
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  if (Slot_is_debuginfo(slot)) {
    /* already a decoded debuginfo */
    return Debuginfo_slot(slot);
  } else {
    return debuginfo_extract(Frame_descr_slot(slot), -1);
  }
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

/* Extended version of name_info including location fields which didn't fit
   in the main debuginfo word. */
struct name_and_loc_info {
  int32_t filename_offs;
  uint16_t start_chr;
  uint16_t end_chr;
  int32_t end_offset; /* End character position relative to start bol */
  char name[1];
};

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
     Two possible formats based on value of bit 63:
     Partially packed format
       |------------- info2 ------------||------------- info1 -------------|
       1 lllllllllllllllllll mmmmmmmmmmmmmmmmmm ffffffffffffffffffffffff k n
      63                  44                 26                        2 1 0
     Fully packed format:
       |-------------- info2 --------------||------------- info1 -------------|
       0 llllllllllll mmm aaaaaa bbbbbbb ooooooooo ffffffffffffffffffffffff k n
      63           51  48     42      35        26                        2 1 0
     n (    1 bit ): 0 if this is the final debuginfo
                     1 if there's another following this one
     k (    1 bit ): 0 if it's a call
                     1 if it's a raise
     f (   24 bits): offset (in 4-byte words) of struct relative to dbg. For
                     partially packed format, f is struct name_and_loc_info;
                     for fully packed format, f is struct name_info.
     m ( 17/3 bits): difference between start line and end line
     o (  0/9 bits): difference between start bol and end bol
     a (  0/6 bits): beginning of character range (relative to start bol)
     b (  0/7 bits): end of character range (relative to end bol)
     l (19/12 bits): start line number
   */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 2) == 2;
  li->loc_is_inlined = caml_debuginfo_next(dbg) != NULL;
  if (info2 & 0x80000000) {
    struct name_and_loc_info * name_and_loc_info =
      (struct name_and_loc_info*)((char *) dbg + (info1 & 0x3FFFFFC));
    li->loc_defname = name_and_loc_info->name;
    li->loc_filename =
      (char *)name_and_loc_info + name_and_loc_info->filename_offs;
    li->loc_start_lnum = li->loc_end_lnum = (info2 >> 12) & 0x7FFFF;
    li->loc_end_lnum += ((info2 & 0xFFF) << 6) | (info1 >> 26);
    li->loc_start_chr = name_and_loc_info->start_chr;
    li->loc_end_chr = name_and_loc_info->end_chr;
    li->loc_end_offset = name_and_loc_info->end_offset;
  } else {
    struct name_info * name_info =
      (struct name_info*)((char *) dbg + (info1 & 0x3FFFFFC));
    li->loc_defname = name_info->name;
    li->loc_filename =
      (char *)name_info + name_info->filename_offs;
    li->loc_start_lnum = li->loc_end_lnum = info2 >> 19;
    li->loc_end_lnum += (info2 >> 16) & 0x7;
    li->loc_start_chr = (info2 >> 10) & 0x3F;
    li->loc_end_chr = li->loc_end_offset = (info2 >> 3) & 0x7F;
    li->loc_end_offset += (((info2 & 0x7) << 6) | (info1 >> 26));
  }
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
