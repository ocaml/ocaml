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
frame_descr * caml_next_frame_descriptor
    (caml_frame_descrs * fds, uintnat * pc, char ** sp,
     struct stack_info* stack)
{
  frame_descr * d;

  while (1) {
    d = caml_find_frame_descr(fds, *pc);

    if( d == NULL ) {
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

int caml_alloc_backtrace_buffer(void){
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
  if (Caml_state->backtrace_buffer == NULL) return -1;
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

static debuginfo debuginfo_extract(frame_descr *d, ptrdiff_t alloc_idx);

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented.

   TODO: Consider rewriting this to use get_callstack, so we only have
   one body of code capturing callstacks.
*/
void caml_stash_backtrace(value exn, uintnat pc, char * sp, const char* trapsp)
{
  caml_domain_state* domain_state = Caml_state;
  caml_frame_descrs* fds;

  if (exn != domain_state->backtrace_last_exn) {
    domain_state->backtrace_pos = 0;
    caml_modify_generational_global_root
      (&domain_state->backtrace_last_exn, exn);
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  fds = caml_get_frame_descrs();
  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor
                                (fds, &pc, &sp, domain_state->current_stack);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (domain_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    domain_state->backtrace_buffer[domain_state->backtrace_pos++] =
      (backtrace_slot) descr;

    /* Stop when we reach the current exception handler */
    if (sp > trapsp) return;
  }
}

/* minimum size to allocate a backtrace (in slots) */
#define MIN_BACKTRACE_SIZE 16

/* Stores up to [max_slots] backtrace slots of the current call stack to
   return to the user in [*backtrace_p] (with the allocated size in
   [*alloc_size_p]). Returns the number of frames stored. Instead of
   using a bounded buffer as [caml_stash_backtrace], we dynamically
   grow the allocated space as required. */
static size_t get_callstack(struct stack_info* stack, intnat max_slots,
                            ptrdiff_t alloc_idx,
                            backtrace_slot **backtrace_p,
                            size_t *alloc_size_p)
{
  backtrace_slot *backtrace = *backtrace_p;
  size_t alloc_size = *alloc_size_p;
  size_t slots = 0;
  char *sp;
  uintnat pc;
  caml_frame_descrs *fds = caml_get_frame_descrs();
  CAMLnoalloc;

  caml_get_stack_sp_pc(stack, &sp, &pc);

  while (slots < max_slots) {
    frame_descr *descr = caml_next_frame_descriptor(fds, &pc, &sp, stack);
    if (!descr) {
      stack = Stack_parent(stack);
      if (!stack) break;
      caml_get_stack_sp_pc(stack, &sp, &pc);
    } else {
      if (slots == alloc_size) {
        size_t new_size = alloc_size ? alloc_size * 2 : MIN_BACKTRACE_SIZE;
        backtrace = caml_stat_resize_noexc(backtrace,
                                           sizeof(backtrace_slot) * new_size);

        if (!backtrace) { /* allocation failed */
          *backtrace_p = NULL;
          *alloc_size_p = 0;
          return 0;
        }
        alloc_size = new_size;
      }

      backtrace_slot slot = Slot_frame_descr(descr);
      if (alloc_idx >= 0) {
        debuginfo info = debuginfo_extract(descr, alloc_idx);
        if (info) {
          CAMLassert(((uintnat)info & 3) == 0); /* so we can tag it */
          slot = Slot_debuginfo(info);
        }
        alloc_idx = -1;
      }
      backtrace[slots++] = slot;
    }
  }

  *alloc_size_p = alloc_size;
  *backtrace_p = backtrace;
  return slots;
}

/* Obtain up to [max_slots] of the callstack of the current domain,
 * including parent fibers. The callstack is written into [*buffer_p],
 * current size [*alloc_size_p], which should be reallocated (on the C
 * heap) if required. Returns the number of slots obtained.
 *
 * If [alloc_idx] is non-negative, then the backtrace is of an
 * allocation point and may therefore include an initial entry of the
 * allocation point itself.
 */

size_t caml_get_callstack(size_t max_slots,
                          backtrace_slot **buffer_p,
                          size_t *alloc_size_p,
                          ptrdiff_t alloc_idx)
{
  return get_callstack(Caml_state->current_stack, max_slots,
                       alloc_idx,
                       buffer_p, alloc_size_p);
}

static value alloc_callstack(backtrace_slot* trace, size_t slots)
{
  CAMLparam0();
  CAMLlocal1(callstack);
  callstack = caml_alloc(slots, 0);
  for (int i = 0; i < slots; i++)
    Store_field(callstack, i, Val_backtrace_slot(trace[i]));
  caml_stat_free(trace);
  CAMLreturn(callstack);
}

/* Create and return a [Printexc.raw_backtrace] of the current
 * callstack, of up to [max_frames_value] entries. Includes parent
 * fibers.
 */

CAMLprim value caml_get_current_callstack (value max_frames_value)
{
  backtrace_slot *trace = NULL;
  size_t trace_size = 0;
  size_t slots = get_callstack(Caml_state->current_stack,
                               Long_val(max_frames_value),
                               -1, &trace, &trace_size);
  return alloc_callstack(trace, slots);
}

/* Create and return a [Printexc.raw_backtrace] of the callstack of
 * the continuation [cont], of up to [max_frames_value]
 * entries. Includes parent fibers.
 */

CAMLprim value caml_get_continuation_callstack (value cont, value max_frames)
{
  backtrace_slot *trace = NULL;
  size_t trace_size = 0;
  size_t slots;
  struct stack_info* stack;

  stack = Ptr_val(caml_continuation_use(cont));
  {
    CAMLnoalloc;
    slots = get_callstack(stack, max_frames, -1,
                          &trace, &trace_size);
    caml_continuation_replace(cont, stack);
  }

  return alloc_callstack(trace, slots);
}

static debuginfo debuginfo_extract(frame_descr *d, ptrdiff_t alloc_idx)
{
  unsigned char* infoptr;
  uint32_t debuginfo_offset;

  /* The special frames marking returns from Caml to C are never
     returned by caml_next_frame_descriptor, so should never reach
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
    if (alloc_idx >= 0) {
      infoptr += alloc_idx * sizeof(uint32_t);
      if (*(uint32_t*)infoptr == 0) {
        /* No debug info for this particular allocation */
        return NULL;
      }
    } else {
      /* we know there's at least one valid debuginfo,
         but it may not be the one for the first alloc */
      while (*(uint32_t*)infoptr == 0) {
        infoptr += sizeof(uint32_t);
      }
    }
  } else {
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    CAMLassert(alloc_idx == -1);
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
  char name[]; /* flexible array member */
};

/* Extended version of name_info including location fields which didn't fit
   in the main debuginfo word. */
struct name_and_loc_info {
  int32_t filename_offs;
  uint16_t start_chr;
  uint16_t end_chr;
  int32_t end_offset; /* End character position relative to start bol */
  char name[]; /* flexible array member */
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
