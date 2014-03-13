/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "mlvalues.h"
#include "alloc.h"
#include "io.h"
#include "instruct.h"
#include "intext.h"
#include "exec.h"
#include "fix_code.h"
#include "memory.h"
#include "startup.h"
#include "stacks.h"
#include "sys.h"
#include "backtrace.h"

CAMLexport int caml_backtrace_active = 0;
CAMLexport int caml_backtrace_pos = 0;
CAMLexport code_t * caml_backtrace_buffer = NULL;
CAMLexport value caml_backtrace_last_exn = Val_unit;
CAMLexport char * caml_cds_file = NULL;
#define BACKTRACE_BUFFER_SIZE 1024

/* Location of fields in the Instruct.debug_event record */
enum { EV_POS = 0,
       EV_MODULE = 1,
       EV_LOC = 2,
       EV_KIND = 3 };

/* Location of fields in the Location.t record. */
enum { LOC_START = 0,
       LOC_END = 1,
       LOC_GHOST = 2 };

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

/* Start or stop the backtrace machinery */

CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    if (flag) {
      caml_register_global_root(&caml_backtrace_last_exn);
    } else {
      caml_remove_global_root(&caml_backtrace_last_exn);
    }
    /* Note: lazy initialization of caml_backtrace_buffer in
       caml_stash_backtrace to simplify the interface with the thread
       libraries */
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(caml_backtrace_active);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, code_t pc, value * sp)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);
  if (pc != NULL) pc = pc - 1;
  if (exn != caml_backtrace_last_exn) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }
  if (caml_backtrace_buffer == NULL) {
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (caml_backtrace_buffer == NULL) return;
  }
  if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  if (pc >= caml_start_code && pc < end_code){
    /* testing the code region is needed: PR#1554 */
    caml_backtrace_buffer[caml_backtrace_pos++] = pc;
  }
  for (/*nothing*/; sp < caml_trapsp; sp++) {
    code_t p = (code_t) *sp;
    if (p >= caml_start_code && p < end_code) {
      if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      caml_backtrace_buffer[caml_backtrace_pos++] = p;
    }
  }
}

/* returns the next frame pointer (or NULL if none is available);
   updates *sp to point to the following one, and *trapsp to the next
   trap frame, which we will skip when we reach it  */

code_t caml_next_frame_pointer(value ** sp, value ** trapsp)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);

  while (*sp < caml_stack_high) {
    code_t *p = (code_t*) (*sp)++;
    if(&Trap_pc(*trapsp) == p) {
      *trapsp = Trap_link(*trapsp);
      continue;
    }
    if (*p >= caml_start_code && *p < end_code) return *p;
  }
  return NULL;
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

CAMLprim value caml_get_current_callstack(value max_frames_value) {
  CAMLparam1(max_frames_value);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  /* first compute the size of the trace */
  {
    value * sp = caml_extern_sp;
    value * trapsp = caml_trapsp;

    for (trace_size = 0; trace_size < max_frames; trace_size++) {
      code_t p = caml_next_frame_pointer(&sp, &trapsp);
      if (p == NULL) break;
    }
  }

  trace = caml_alloc(trace_size, Abstract_tag);

  /* then collect the trace */
  {
    value * sp = caml_extern_sp;
    value * trapsp = caml_trapsp;
    uintnat trace_pos;

    for (trace_pos = 0; trace_pos < trace_size; trace_pos++) {
      code_t p = caml_next_frame_pointer(&sp, &trapsp);
      Assert(p != NULL);
      /* The assignment below is safe without [caml_initialize], even
         if the trace is large and allocated on the old heap, because
         we assign values that are outside the OCaml heap. */
      Assert(!(Is_block((value) p) && Is_in_heap((value) p)));
      Field(trace, trace_pos) = (value) p;
    }
  }

  CAMLreturn(trace);
}

/* Read the debugging info contained in the current bytecode executable.
   Return an OCaml array of OCaml lists of debug_event records in "events",
   or Val_false on failure. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

static char *read_debug_info_error = "";
static value read_debug_info(void)
{
  CAMLparam0();
  CAMLlocal1(events);
  char * exec_name;
  int fd;
  struct exec_trailer trail;
  struct channel * chan;
  uint32 num_events, orig, i;
  value evl, l;

  if (caml_cds_file != NULL) {
    exec_name = caml_cds_file;
  } else {
    exec_name = caml_exe_name;
  }
  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0){
    read_debug_info_error = "executable program file not found";
    CAMLreturn(Val_false);
  }
  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
    read_debug_info_error = "program not linked with -g";
    CAMLreturn(Val_false);
  }
  chan = caml_open_descriptor_in(fd);
  num_events = caml_getword(chan);
  events = caml_alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = caml_getword(chan);
    evl = caml_input_val(chan);
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field(l, 1)) {
      value ev = Field(l, 0);
      Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
    }
    /* Record event list */
    Store_field(events, i, evl);
  }
  caml_close_channel(chan);
  CAMLreturn(events);
}

/* Search the event for the given PC.  Return Val_false if not found. */

static value event_for_location(value events, code_t pc)
{
  mlsize_t i;
  value pos, l, ev, ev_pos, best_ev;

  best_ev = 0;
  Assert(pc >= caml_start_code && pc < caml_start_code + caml_code_size);
  pos = Val_long((char *) pc - (char *) caml_start_code);
  for (i = 0; i < Wosize_val(events); i++) {
    for (l = Field(events, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);
      ev_pos = Field(ev, EV_POS);
      if (ev_pos == pos) return ev;
      /* ocamlc sometimes moves an event past a following PUSH instruction;
         allow mismatch by 1 instruction. */
      if (ev_pos == pos + 8) best_ev = ev;
    }
  }
  if (best_ev != 0) return best_ev;
  return Val_false;
}

/* Extract location information for the given PC */

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

static void extract_location_info(value events, code_t pc,
                                  /*out*/ struct loc_info * li)
{
  value ev, ev_start;

  ev = event_for_location(events, pc);
  li->loc_is_raise = caml_is_instruction(*pc, RAISE);
  if (ev == Val_false) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  ev_start = Field (Field (ev, EV_LOC), LOC_START);
  li->loc_filename = String_val (Field (ev_start, POS_FNAME));
  li->loc_lnum = Int_val (Field (ev_start, POS_LNUM));
  li->loc_startchr =
    Int_val (Field (ev_start, POS_CNUM))
    - Int_val (Field (ev_start, POS_BOL));
  li->loc_endchr =
    Int_val (Field (Field (Field (ev, EV_LOC), LOC_END), POS_CNUM))
    - Int_val (Field (ev_start, POS_BOL));
}

/* Print location information -- same behavior as in Printexc */

static void print_location(struct loc_info * li, int index)
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

CAMLexport void caml_print_exception_backtrace(void)
{
  value events;
  int i;
  struct loc_info li;

  events = read_debug_info();
  if (events == Val_false) {
    fprintf(stderr, "(Cannot print stack backtrace: %s)\n",
            read_debug_info_error);
    return;
  }
  for (i = 0; i < caml_backtrace_pos; i++) {
    extract_location_info(events, caml_backtrace_buffer[i], &li);
    print_location(&li, i);
  }
}

/* Convert the backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace(value backtrace)
{
  CAMLparam1(backtrace);
  CAMLlocal5(events, res, arr, p, fname);
  int i;
  struct loc_info li;

  events = read_debug_info();
  if (events == Val_false) {
    res = Val_int(0);           /* None */
  } else {
    arr = caml_alloc(Wosize_val(backtrace), 0);
    for (i = 0; i < Wosize_val(backtrace); i++) {
      extract_location_info(events, (code_t)Field(backtrace, i), &li);
      if (li.loc_valid) {
        fname = caml_copy_string(li.loc_filename);
        p = caml_alloc_small(5, 0);
        Field(p, 0) = Val_bool(li.loc_is_raise);
        Field(p, 1) = fname;
        Field(p, 2) = Val_int(li.loc_lnum);
        Field(p, 3) = Val_int(li.loc_startchr);
        Field(p, 4) = Val_int(li.loc_endchr);
      } else {
        p = caml_alloc_small(1, 1);
        Field(p, 0) = Val_bool(li.loc_is_raise);
      }
      caml_modify(&Field(arr, i), p);
    }
    res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  }
  CAMLreturn(res);
}

/* Get a copy of the latest backtrace */

CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc(caml_backtrace_pos, Abstract_tag);
  if(caml_backtrace_buffer != NULL)
    memcpy(&Field(res, 0), caml_backtrace_buffer,
           caml_backtrace_pos * sizeof(code_t));
  CAMLreturn(res);
}

/* the function below is deprecated: see asmrun/backtrace.c */

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal2(raw, res);
  raw = caml_get_exception_raw_backtrace(unit);
  res = caml_convert_raw_backtrace(raw);
  CAMLreturn(res);
}
