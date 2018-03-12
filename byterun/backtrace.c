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

/* TODO KC: Split into backtrace_prim.c */

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/io.h"
#include "caml/instruct.h"
#include "caml/intext.h"
#include "caml/exec.h"
#include "caml/fix_code.h"
#include "caml/memory.h"
#include "caml/startup.h"
#include "caml/fiber.h"
#include "caml/sys.h"
#include "caml/backtrace.h"
#include "caml/fail.h"

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
    /* Note: lazy initialization of caml_backtrace_buffer in
       caml_stash_backtrace to simplify the interface with the thread
       libraries */
  }
  return Val_unit;
}

/* Return the status of the backtrace machinery */

CAMLprim value caml_backtrace_status(value vunit)
{
  return Val_bool(Caml_state->backtrace_active);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, code_t pc, value * sp, int reraise)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);
  if (pc != NULL) pc = pc - 1;
  if (exn != caml_read_root(Caml_state->backtrace_last_exn) || !reraise) {
    Caml_state->backtrace_pos = 0;
    caml_modify_root(Caml_state->backtrace_last_exn, exn);
  }
  if (Caml_state->backtrace_buffer == NULL) {
    Assert(Caml_state->backtrace_pos == 0);
    Caml_state->backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (Caml_state->backtrace_buffer == NULL) return;
  }
  if (Caml_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  if (pc >= caml_start_code && pc < end_code){
    /* testing the code region is needed: PR#1554 */
    Caml_state->backtrace_buffer[Caml_state->backtrace_pos++] = pc;
  }
  for (/*nothing*/; sp < Caml_state->stack_high + Caml_state->trap_sp_off; sp++) {
    if (Is_long(*sp) && Pc_val(*sp) >= caml_start_code && Pc_val(*sp) < end_code) {
      if (Caml_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      Caml_state->backtrace_buffer[Caml_state->backtrace_pos++] = Pc_val(*sp);
    }
  }
}

/* In order to prevent the GC from walking through the debug
   information (which have no headers), we transform code pointers to
   31/63 bits ocaml integers by shifting them by 1 to the right. We do
   not lose information as code pointers are aligned.

   In particular, we do not need to use [caml_initialize] when setting
   an array element with such a value.
*/
#define Val_Codet(p) Val_long((uintnat)p>>1)
#define Codet_Val(v) ((code_t)(Long_val(v)<<1))

/* returns the next frame pointer (or NULL if none is available);
   updates *sp to point to the following one, and *trap_spoff to the next
   trap frame, which we will skip when we reach it  */

static code_t next_frame_pointer(value* stack_high, value ** sp,
                                 intnat * trap_spoff)
{
  code_t end_code = (code_t) ((char *) caml_start_code + caml_code_size);

  while (*sp < stack_high) {
    value* p = (*sp)++;
    if(&Trap_pc(stack_high + *trap_spoff) == p) {
      *trap_spoff = Trap_link(stack_high + *trap_spoff);
      continue;
    }
    if (Is_long(*p) &&
        Pc_val(*p) >= caml_start_code &&
        Pc_val(*p) < end_code) {
      return Pc_val(*p);
    }
  }
  return NULL;
}

/* Stores upto [max_frames_value] frames of the current call stack to
   return to the user. This is used not in an exception-raising
   context, but only when the user requests to save the trace
   (hopefully less often). Instead of using a bounded buffer as
   [caml_stash_backtrace], we first traverse the stack to compute the
   right size, then allocate space for the trace. */

value get_callstack(value* sp, intnat trap_spoff, value stack,
                    value max_frames_value)
{
  CAMLparam2(max_frames_value, stack);
  CAMLlocal1(trace);

  /* we use `intnat` here because, were it only `int`, passing `max_int`
     from the OCaml side would overflow on 64bits machines. */
  intnat max_frames = Long_val(max_frames_value);
  intnat trace_size;

  value parent = Stack_parent(stack);
  value *stack_high = Stack_high(stack);
  value* saved_sp = sp;
  intnat saved_trap_spoff = trap_spoff;

  /* first compute the size of the trace */
  {
    trace_size = 0;
    while (trace_size < max_frames) {
      code_t p = next_frame_pointer(stack_high, &sp, &trap_spoff);
      if (p == NULL) {
        if (parent == Val_unit) break;
        sp = Stack_high(parent) + Stack_sp(parent);
        trap_spoff = Long_val(sp[0]);
        stack_high = Stack_high(parent);
        parent = Stack_parent(parent);
      } else {
        ++trace_size;
      }
    }
  }

  trace = caml_alloc(trace_size, 0);

  sp = saved_sp;
  parent = Stack_parent(stack);
  stack_high = Stack_high(stack);
  trap_spoff = saved_trap_spoff;

  /* then collect the trace */
  {
    uintnat trace_pos = 0;

    while (trace_pos < trace_size) {
      code_t p = next_frame_pointer(stack_high, &sp, &trap_spoff);
      if (p == NULL) {
        sp = Stack_high(parent) + Stack_sp(parent);
        trap_spoff = Long_val(sp[0]);
        stack_high = Stack_high(parent);
        parent = Stack_parent(parent);
      } else {
        caml_initialize_field(trace, trace_pos, Val_Codet(p));
        ++trace_pos;
      }
    }
  }

  CAMLreturn(trace);
}

CAMLprim value caml_get_current_callstack (value max_frames_value)
{
  CAMLparam1(max_frames_value);
  CAMLlocal2(stack, callstack);
  caml_domain_state* domain_state = Caml_state;

  callstack =
    get_callstack (domain_state->extern_sp, domain_state->trap_sp_off,
                   domain_state->current_stack, max_frames_value);

  CAMLreturn(callstack);
}

CAMLprim value caml_get_continuation_callstack (value cont, value max_frames)
{
  CAMLparam1(cont);
  CAMLlocal2(stack, callstack);
  intnat bvar_stat;
  value *sp;

  bvar_stat = caml_bvar_status(cont);
  if (bvar_stat & BVAR_EMPTY)
    caml_invalid_argument ("continuation already taken");

  caml_read_field(cont, 0, &stack);

  stack = caml_reverse_fiber_stack(stack);
  sp = Stack_high(stack) + Stack_sp(stack);
  callstack = get_callstack (sp, Long_val(sp[0]), stack, max_frames);
  caml_reverse_fiber_stack(stack);

  CAMLreturn(callstack);
}

/* Read the debugging info contained in the current bytecode executable. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

struct ev_info {
  code_t ev_pc;
  char * ev_filename;
  int ev_lnum;
  int ev_startchr;
  int ev_endchr;
};

static int cmp_ev_info(const void *a, const void *b) {
  code_t pc_a = ((const struct ev_info*)a)->ev_pc;
  code_t pc_b = ((const struct ev_info*)b)->ev_pc;
  if (pc_a > pc_b) return 1;
  if (pc_a < pc_b) return -1;
  return 0;
}

static __thread char *read_debug_info_error = "";
static __thread uintnat n_events;
static __thread struct ev_info *events = NULL;
static void read_debug_info()
{
  CAMLparam0();
  CAMLlocal1(events_heap);
  const char * exec_name;
  int fd;
  struct exec_trailer trail;
  struct channel * chan;
  uint32_t num_events, orig, i;
  intnat j;
  value evl, l, ev_start;

  if(events != NULL)
    CAMLreturn0;

  if (caml_params->cds_file != NULL) {
    exec_name = caml_params->cds_file;
  } else {
    exec_name = caml_params->exe_name;
  }
  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0){
    read_debug_info_error = "executable program file not found";
    CAMLreturn0;
  }
  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
    read_debug_info_error = "program not linked with -g";
    CAMLreturn0;
  }
  chan = caml_open_descriptor_in(fd);
  num_events = caml_getword(chan);
  n_events = 0;
  events_heap = caml_alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = caml_getword(chan);
    evl = caml_input_val(chan);
    caml_input_val(chan); // Skip the list of absolute directory names
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field_imm(l, 1)) {
      value ev = Field_imm(l, 0);
      Store_field(ev, EV_POS, Val_long(Long_field(ev, EV_POS) + orig));
      n_events++;
    }
    /* Record event list */
    Store_field(events_heap, i, evl);
  }
  caml_close_channel(chan);

  events = (struct ev_info*)malloc(n_events * sizeof(struct ev_info));
  if(events == NULL) {
    read_debug_info_error = "out of memory";
    CAMLreturn0;
  }

  j = 0;
  for (i = 0; i < num_events; i++) {
    for (l = Field_imm(events_heap, i); l != Val_int(0); l = Field_imm(l, 1)) {
      uintnat fnsz;
      value ev = Field_imm(l, 0);

      events[j].ev_pc =
        (code_t)((char*)caml_start_code + Long_val(Field_imm(ev, EV_POS)));

      ev_start = Field_imm(Field_imm(ev, EV_LOC), LOC_START);

      fnsz = caml_string_length(Field_imm(ev_start, POS_FNAME))+1;
      events[j].ev_filename = (char*)malloc(fnsz);
      if(events[j].ev_filename == NULL) {
        for(j--; j >= 0; j--)
          free(events[j].ev_filename);
        free(events);
        events = NULL;
        read_debug_info_error = "out of memory";
        CAMLreturn0;
      }
      memcpy(events[j].ev_filename, String_val (Field_imm(ev_start, POS_FNAME)),
             fnsz);

      events[j].ev_lnum = Int_val (Field_imm(ev_start, POS_LNUM));
      events[j].ev_startchr =
        Int_val (Field_imm(ev_start, POS_CNUM))
        - Int_val (Field_imm(ev_start, POS_BOL));
      events[j].ev_endchr =
        Int_val (Field_imm(Field_imm(Field_imm(ev, EV_LOC), LOC_END), POS_CNUM))
        - Int_val (Field_imm(ev_start, POS_BOL));

      j++;
    }
  }

  Assert(j == n_events);

  qsort(events, n_events, sizeof(struct ev_info), cmp_ev_info);

  CAMLreturn0;
}

/* Search the event index for the given PC.  Return -1 if not found. */

static intnat event_for_location(code_t pc)
{
  uintnat low = 0, high = n_events;
  Assert(pc >= caml_start_code && pc < caml_start_code + caml_code_size);
  Assert(events != NULL);
  while(low+1 < high) {
    uintnat m = (low+high)/2;
    if(pc < events[m].ev_pc) high = m;
    else low = m;
  }
  if(events[low].ev_pc == pc)
    return low;
  /* ocamlc sometimes moves an event past a following PUSH instruction;
     allow mismatch by 1 instruction. */
  if(events[low].ev_pc == pc + 1)
    return low;
  if(low+1 < n_events && events[low+1].ev_pc == pc + 1)
    return low+1;
  return -1;
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

static void extract_location_info(code_t pc,
                                  /*out*/ struct loc_info * li)
{
  intnat ev = event_for_location(pc);
  li->loc_is_raise =
    caml_is_instruction(*pc, RAISE) ||
    caml_is_instruction(*pc, RERAISE);
  if (ev == -1) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  li->loc_filename = events[ev].ev_filename;
  li->loc_lnum = events[ev].ev_lnum;
  li->loc_startchr = events[ev].ev_startchr;
  li->loc_endchr = events[ev].ev_endchr;
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
  intnat i;
  struct loc_info li;

  read_debug_info();
  if (events == NULL) {
    fprintf(stderr, "(Cannot print stack backtrace: %s)\n",
            read_debug_info_error);
    return;
  }
  for (i = 0; i < Caml_state->backtrace_pos; i++) {
    extract_location_info(Caml_state->backtrace_buffer[i], &li);
    print_location(&li, i);
  }
}

/* Convert the backtrace to a data structure usable from OCaml */

CAMLprim value caml_convert_raw_backtrace_slot(value backtrace_slot) {
  CAMLparam1(backtrace_slot);
  CAMLlocal2(p, fname);
  struct loc_info li;

  read_debug_info();
  if (events == NULL)
    caml_failwith(read_debug_info_error);

  extract_location_info(Codet_Val(backtrace_slot), &li);

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

  res = caml_alloc(Caml_state->backtrace_pos, 0);
  if(Caml_state->backtrace_buffer != NULL) {
    intnat i;
    for(i = 0; i < Caml_state->backtrace_pos; i++)
      caml_initialize_field(res, i, Val_Codet(Caml_state->backtrace_buffer[i]));
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
  CAMLlocal4(arr, raw_slot, slot, res);

  read_debug_info();
  if (events == NULL) {
      res = Val_int(0); /* None */
  } else {
    arr = caml_alloc(Caml_state->backtrace_pos, 0);
    if(Caml_state->backtrace_buffer == NULL) {
        Assert(Caml_state->backtrace_pos == 0);
    } else {
        intnat i;
        for(i = 0; i < Caml_state->backtrace_pos; i++) {
            raw_slot = Val_Codet(Caml_state->backtrace_buffer[i]);
            /* caml_convert_raw_backtrace_slot will not fail with
              caml_failwith as we checked (events != NULL) already */
            slot = caml_convert_raw_backtrace_slot(raw_slot);
            caml_modify_field(arr, i, slot);
        }
    }
    res = caml_alloc_small(1, 0); caml_initialize_field(res, 0, arr); /* Some */
  }
  CAMLreturn(res);
}

CAMLprim value caml_add_debug_info(code_t start, value size,
                                   value events)
{
  return Val_unit;
}

CAMLprim value caml_remove_debug_info(code_t start)
{
  return Val_unit;
}

CAMLprim value caml_raw_backtrace_length(value bt)
{
  return Val_int(Wosize_val(bt));
}

CAMLprim value caml_raw_backtrace_slot(value bt, value index)
{
  /* TODO KC */
  caml_failwith ("caml_raw_backtrace_next_slot not implemented");
  return Val_unit;
}

CAMLprim value caml_raw_backtrace_next_slot(value slot)
{
  /* TODO KC */
  caml_failwith ("caml_raw_backtrace_next_slot not implemented");
  return Val_unit;
}

/* Convert the raw backtrace to a data structure usable from OCaml */
CAMLprim value caml_convert_raw_backtrace(value bt)
{
  /* TODO KC */
  caml_failwith ("caml_raw_backtrace_next_slot not implemented");
  return Val_unit;
}
