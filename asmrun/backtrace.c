/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2006 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "stack.h"
#include "frame_descriptors.h"

#define BACKTRACE_BUFFER_SIZE 1024

/* In order to prevent the GC from walking through the debug information
   (which have no headers), we transform frame_descr pointers into
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as descr pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Descrptr(descr) Val_long((uintnat)descr>>1)
#define Descrptr_Val(v) ((frame_descr *) (Long_val(v)<<1))

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  intnat flag = Int_val(vflag);

  if (flag != Caml_state->backtrace_active) {
    Caml_state->backtrace_active = flag;
    Caml_state->backtrace_pos = 0;
    if (flag) {
      Caml_state->backtrace_last_exn = caml_create_root(Val_unit);
    } else {
      caml_delete_root(Caml_state->backtrace_last_exn);
      Caml_state->backtrace_last_exn = NULL;
    }
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(Caml_state->backtrace_active);
}

/* returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */

frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;

  while (1) {
    d = caml_find_frame_descr(*pc);
    if (d == NULL) return NULL;
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
#ifndef Stack_grows_upwards
      *sp += (d->frame_size & 0xFFFC);
#else
      *sp -= (d->frame_size & 0xFFFC);
#endif
      *pc = Saved_return_address(*sp);
      return d;
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      *sp = (char*)Caml_state->stack_high;
      *pc = 0;
      return d;
    }
  }
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */

void caml_stash_backtrace(value exn, uintnat pc, char * sp, uintnat trapsp_off)
{
  caml_domain_state* domain_state = Caml_state;
  char* stack_high = (char*)domain_state->stack_high;

  if (exn != caml_read_root(domain_state->backtrace_last_exn)) {
    domain_state->backtrace_pos = 0;
    caml_modify_root(domain_state->backtrace_last_exn, exn);
  }
  if (domain_state->backtrace_buffer == NULL) {
    Assert(domain_state->backtrace_pos == 0);
    domain_state->backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (domain_state->backtrace_buffer == NULL) return;
  }

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (domain_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    domain_state->backtrace_buffer[domain_state->backtrace_pos++] = (code_t) descr;

    /* Stop when we reach the current exception handler */
#ifndef Stack_grows_upwards
    if (sp > stack_high - trapsp_off) return;
#else
    if (sp < stack_high - trapsp_off) return;
#endif
  }
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

static value get_callstack(value stack, value max_frames_value) {
  CAMLparam2(stack, max_frames_value);
  CAMLlocal2(saved_stack, trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_pos;
  char *sp;
  uintnat pc;

  saved_stack = stack;
  /* first compute the size of the trace */
  {
    caml_get_stack_sp_pc(stack, &sp, &pc);
    trace_pos = 0;

    while(1) {
      frame_descr *descr = caml_next_frame_descriptor(&pc, &sp);
      if (descr == NULL) break;
      if (trace_pos >= max_frames) break;
      if (descr->frame_size == 0xFFFF) {
        stack = Stack_parent(stack);
        if (stack == Val_unit) break;
        caml_get_stack_sp_pc(stack, &sp, &pc);
      } else {
        ++trace_pos;
      }
    }
  }

  trace = caml_alloc((mlsize_t) trace_pos, 0);
  stack = saved_stack;

  /* then collect the trace */
  {
    caml_get_stack_sp_pc(stack, &sp, &pc);
    trace_pos = 0;

    while(1) {
      frame_descr *descr = caml_next_frame_descriptor(&pc, &sp);
      if (descr == NULL) break;
      if (trace_pos >= max_frames) break;
      if (descr->frame_size == 0xFFFF) {
        stack = Stack_parent(stack);
        if (stack == Val_unit) break;
        caml_get_stack_sp_pc(stack, &sp, &pc);
      } else {
        caml_modify_field(trace, trace_pos, Val_Descrptr(descr));
        ++trace_pos;
      }
    }
  }

  CAMLreturn(trace);
}

CAMLprim value caml_get_current_callstack (value max_frames_value) {
  caml_domain_state* domain_state = Caml_state;
  return get_callstack(domain_state->current_stack, max_frames_value);
}

CAMLprim value caml_get_continuation_callstack (value cont, value max_frames)
{
  CAMLparam1(cont);
  CAMLlocal2(stack, callstack);
  intnat bvar_stat;

  bvar_stat = caml_bvar_status(cont);
  if (bvar_stat & BVAR_EMPTY)
    caml_invalid_argument ("continuation already taken");

  caml_read_field(cont, 0, &stack);

  stack = caml_reverse_fiber_stack(stack);
  callstack = get_callstack (stack, max_frames);
  caml_reverse_fiber_stack(stack);

  CAMLreturn(callstack);
}

/* Extract location information for the given frame descriptor */

struct caml_loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};


CAMLexport void extract_location_info(frame_descr * d,
                                  /*out*/ struct caml_loc_info * li)
{
  uintnat infoptr;
  uint32_t info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }

  if (d->frame_size == 0xFFFF) {
    /* Special frame marking the top of a stack chunk for an ML callback. */
    /* XXX KC: Should the handlers be made distinct from compiler-inserted
     * re-raise operations (above)? The backtrace ignores them currently. */
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }

  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  info1 = ((uint32_t *)infoptr)[0];
  info2 = ((uint32_t *)infoptr)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call, 1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) != 0;
  li->loc_filename = (char *) infoptr + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   extract_location_info above guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the byterun/
   implementation. */

static void print_location(struct caml_loc_info * li, int index)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid && li->loc_is_raise) return;

  if (li->loc_is_raise) {
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    if (index == 0)
      info = "Raised by primitive operation at";
    else
      info = "Called from";
  }
  if (! li->loc_valid) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf (stderr, "%s file \"%s\", line %d, characters %d-%d\n",
             info, li->loc_filename, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* Print a backtrace */

void caml_print_exception_backtrace(void)
{
  intnat i;
  struct caml_loc_info li;

  for (i = 0; i < Caml_state->backtrace_pos; i++) {
    extract_location_info((frame_descr *) (Caml_state->backtrace_buffer[i]), &li);
    print_location(&li, i);
  }
}

/* Convert the raw backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  extract_location_info(Descrptr_Val(backtrace_slot), &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(5, 0);
    caml_initialize_field(p, 0, Val_bool(li.loc_is_raise));
    caml_initialize_field(p, 1, fname);
    caml_initialize_field(p, 2, Val_int(li.loc_lnum));
    caml_initialize_field(p, 3, Val_int(li.loc_startchr));
    caml_initialize_field(p, 4, Val_int(li.loc_endchr));
  } else {
    p = caml_alloc_small(1, 1);
    caml_initialize_field(p, 0, Val_bool(li.loc_is_raise));
  }

  CAMLreturn(p);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  const int tag = 0;

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (Caml_state->backtrace_buffer == NULL || Caml_state->backtrace_pos == 0) {
    res = caml_alloc(0, tag);
  }
  else {
    code_t saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    intnat saved_backtrace_pos;
    intnat i;

    saved_backtrace_pos = Caml_state->backtrace_pos;

    if (saved_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    memcpy(saved_caml_backtrace_buffer, Caml_state->backtrace_buffer,
           saved_backtrace_pos * sizeof(code_t));

    res = caml_alloc(saved_backtrace_pos, tag);
    for (i = 0; i < saved_backtrace_pos; i++) {
      /* [Val_Descrptr] always returns an immediate. */
      caml_initialize_field(res, i, Val_Descrptr(saved_caml_backtrace_buffer[i]));
    }
  }

  CAMLreturn(res);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.
*/

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  backtrace = caml_get_exception_raw_backtrace(Val_unit);

  arr = caml_alloc(Wosize_val(backtrace), 0);
  for (i = 0; i < Wosize_val(backtrace); i++) {
    Store_field(arr, i, caml_convert_raw_backtrace_slot(Field_imm(backtrace, i)));
  }

  res = caml_alloc_small(1, 0); caml_initialize_field(res, 0, arr); /* Some */
  CAMLreturn(res);
}
