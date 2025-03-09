/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef _WIN32
#include <io.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/custom.h"
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
#include "caml/backtrace_prim.h"
#include "caml/debugger.h"

/* The table of debug information fragments */
struct ext_table caml_debug_info;

CAMLexport char_os * caml_cds_file = NULL;

/* Location of fields in the Instruct.debug_event record */
enum {
  EV_POS = 0,
  EV_MODULE = 1,
  EV_LOC = 2,
  EV_KIND = 3,
  EV_DEFNAME = 4
};

/* Location of fields in the Location.t record. */
enum {
  LOC_START = 0,
  LOC_END = 1,
  LOC_GHOST = 2
};

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

struct debug_info {
  code_t start;
  code_t end;
  mlsize_t num_events;
  struct ev_info *events;
  int already_read;
};

static struct debug_info *find_debug_info(code_t pc)
{
  for (int i = 0; i < caml_debug_info.size; i++) {
    struct debug_info *di = caml_debug_info.contents[i];
    if (pc >= di->start && pc < di->end)
      return di;
  }
  return NULL;
}

static int cmp_ev_info(const void *a, const void *b)
{
  const struct ev_info* ev_a = a;
  const struct ev_info* ev_b = b;
  code_t pc_a = ev_a->ev_pc;
  code_t pc_b = ev_b->ev_pc;
  int num_a;
  int num_b;

  /* Perform a full lexicographic comparison to make sure the resulting order is
     the same under all implementations of qsort (which is not stable).
     NB a->ev_end_offset == b->ev_end_offset =>
           a->ev_end_lnum == b->ev_end_lnum && a->ev_end_chr == b->ev_end_chr */

  if (pc_a > pc_b) return 1;
  if (pc_a < pc_b) return -1;

  num_a = ev_a->ev_start_lnum;
  num_b = ev_b->ev_start_lnum;

  if (num_a > num_b) return 1;
  if (num_a < num_b) return -1;

  num_a = ev_a->ev_start_chr;
  num_b = ev_b->ev_start_chr;

  if (num_a > num_b) return 1;
  if (num_a < num_b) return -1;

  num_a = ev_a->ev_end_offset;
  num_b = ev_b->ev_end_offset;

  if (num_a > num_b) return 1;
  if (num_a < num_b) return -1;

  return 0;
}

static struct ev_info *process_debug_events(code_t code_start,
                                            value events_heap,
                                            mlsize_t *num_events)
{
  CAMLparam1(events_heap);
  CAMLlocal4(l, ev, ev_start, ev_end);
  mlsize_t j;
  struct ev_info *events;

  /* Compute the size of the required event buffer. */
  *num_events = 0;
  for (mlsize_t i = 0; i < caml_array_length(events_heap); i++)
    for (l = Field(events_heap, i); l != Val_int(0); l = Field(l, 1))
      (*num_events)++;

  if (*num_events == 0)
      CAMLreturnT(struct ev_info *, NULL);

  events = caml_stat_alloc_noexc(*num_events * sizeof(struct ev_info));
  if(events == NULL)
    caml_fatal_error ("caml_add_debug_info: out of memory");

  j = 0;
  for (mlsize_t i = 0; i < caml_array_length(events_heap); i++) {
    for (l = Field(events_heap, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);

      events[j].ev_pc = (code_t)((char*)code_start
                                 + Long_val(Field(ev, EV_POS)));

      ev_start = Field(Field(ev, EV_LOC), LOC_START);
      ev_end = Field(Field(ev, EV_LOC), LOC_END);

      {
        const char *fname = String_val(Field(ev_start, POS_FNAME));
        events[j].ev_filename = caml_stat_strdup_noexc(fname);
        if(events[j].ev_filename == NULL)
          caml_fatal_error ("caml_add_debug_info: out of memory");
      }

      if (Is_block(Field(ev, EV_DEFNAME)) &&
          Tag_val(Field(ev, EV_DEFNAME)) == String_tag) {
        const char *dname = String_val(Field(ev, EV_DEFNAME));
        events[j].ev_defname = caml_stat_strdup_noexc(dname);
        if (events[j].ev_defname == NULL)
          caml_fatal_error ("caml_add_debug_info: out of memory");
      } else {
        events[j].ev_defname = "<old bytecode>";
      }

      events[j].ev_start_lnum = Int_val(Field(ev_start, POS_LNUM));
      events[j].ev_start_chr =
        Int_val(Field(ev_start, POS_CNUM)) - Int_val(Field(ev_start, POS_BOL));
      events[j].ev_end_lnum = Int_val(Field(ev_end, POS_LNUM));
      events[j].ev_end_chr =
        Int_val(Field(ev_end, POS_CNUM)) - Int_val(Field(ev_end, POS_BOL));
      events[j].ev_end_offset =
        Int_val(Field(ev_end, POS_CNUM)) - Int_val(Field(ev_start, POS_BOL));

      j++;
    }
  }

  CAMLassert(j == *num_events);

  qsort(events, *num_events, sizeof(struct ev_info), cmp_ev_info);

  CAMLreturnT(struct ev_info *, events);
}

/* Processes a (Instruct.debug_event list array) into a form suitable
   for quick lookup and registers it for the (code_start,code_size) pc range. */
value caml_add_debug_info(code_t code_start, value code_size, value events_heap)
{
  CAMLparam1(events_heap);
  struct debug_info *debug_info;

  if (events_heap != Val_unit)
    caml_debugger(DEBUG_INFO_ADDED, events_heap);

  /* build the OCaml-side debug_info value */
  debug_info = caml_stat_alloc(sizeof(struct debug_info));

  debug_info->start = code_start;
  debug_info->end = (code_t)((char*) code_start + Long_val(code_size));
  if (events_heap == Val_unit) {
    debug_info->events = NULL;
    debug_info->num_events = 0;
    debug_info->already_read = 0;
  } else {
    debug_info->events =
      process_debug_events(code_start, events_heap, &debug_info->num_events);
    debug_info->already_read = 1;
  }

  caml_ext_table_add(&caml_debug_info, debug_info);

  CAMLreturn(Val_unit);
}

value caml_remove_debug_info(code_t start)
{
  CAMLparam0();
  CAMLlocal2(dis, prev);

  for (int i = 0; i < caml_debug_info.size; i++) {
    struct debug_info *di = caml_debug_info.contents[i];
    if (di->start == start) {
      /* note that caml_ext_table_remove calls caml_stat_free on the
         removed resource, bracketing the caml_stat_alloc call in
         caml_add_debug_info. */
      caml_ext_table_remove(&caml_debug_info, di);
      break;
    }
  }

  CAMLreturn(Val_unit);
}

int caml_alloc_backtrace_buffer (void)
{
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
  if (Caml_state->backtrace_buffer == NULL) return -1;
  return 0;
}

void caml_free_backtrace_buffer(backtrace_slot *backtrace_buffer) {
  if (backtrace_buffer != NULL)
    caml_stat_free(backtrace_buffer);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, value * sp, int reraise)
{
  value *trap_sp;

  if (exn != Caml_state->backtrace_last_exn || !reraise) {
    Caml_state->backtrace_pos = 0;
    caml_modify_generational_global_root(&Caml_state->backtrace_last_exn, exn);
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  /* Traverse the stack and put all values pointing into bytecode
     into the backtrace buffer. */
  trap_sp = Stack_high(Caml_state->current_stack) + Caml_state->trap_sp_off;
  for (/*nothing*/; sp < trap_sp; sp++) {
    code_t p;
    if (Is_long(*sp)) continue;
    p = (code_t) *sp;
    if (Caml_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
    if (find_debug_info(p) != NULL)
      Caml_state->backtrace_buffer[Caml_state->backtrace_pos++] = p;
  }
}

/* returns the next frame pointer (or NULL if none is available);
   updates *sp to point to the following one, and *trap_spoff to the next
   trap frame, which we will skip when we reach it  */

code_t caml_next_frame_pointer(value* stack_high, value ** sp,
                          intnat * trap_spoff)
{
  while (*sp < stack_high) {
    value *spv = (*sp)++;
    code_t *p;
    if (Is_long(*spv)) continue;
    p = (code_t*) spv;
    if((code_t*)&Trap_pc(stack_high + *trap_spoff) == p) {
      *trap_spoff = Trap_link(stack_high + *trap_spoff);
      continue;
    }

    if (find_debug_info(*p) != NULL)
      return *p;
  }
  return NULL;
}

/* minimum size to allocate a backtrace (in slots) */
#define MIN_BACKTRACE_SIZE 16

/* Stores up to [max_slots] backtrace slots of the current call stack
   to return to the user in [*backtrace_p] (with the allocated size in
   [*alloc_size_p]). Returns the number of frames stored.  Instead of
   using a bounded buffer as [Caml_state->stash_backtrace], we
   dynamically grow the allocated space as required. */

static size_t get_callstack(value* sp, intnat trap_spoff,
                            struct stack_info* stack,
                            intnat max_slots,
                            backtrace_slot **backtrace_p,
                            size_t *alloc_size_p)
{
  backtrace_slot *backtrace = *backtrace_p;
  size_t alloc_size = *alloc_size_p;
  struct stack_info* parent = Stack_parent(stack);
  value *stack_high = Stack_high(stack);
  CAMLnoalloc;

  size_t slots = 0;
  while (slots < max_slots) {
    code_t p = caml_next_frame_pointer(stack_high, &sp, &trap_spoff);
    if (!p) {
      if (!parent) break;
      sp = parent->sp;
      trap_spoff = Long_val(sp[0]);
      stack_high = Stack_high(parent);
      parent = Stack_parent(parent);
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

      backtrace[slots++] = p;
    }
  }
  *backtrace_p = backtrace;
  *alloc_size_p = alloc_size;
  return slots;
}

static value alloc_callstack(backtrace_slot *trace, size_t slots)
{
  CAMLparam0();
  CAMLlocal1(callstack);
  callstack = caml_alloc(slots, 0);
  for (int i = 0; i < slots; i++)
    Store_field(callstack, i, Val_backtrace_slot(trace[i]));
  caml_stat_free(trace);
  CAMLreturn(callstack);
}

/* Obtain up to [max_slots] of the callstack of the current domain,
 * including parent fibers. The callstack is written into [*buffer_p],
 * current size [*alloc_size_p], which should be reallocated (on the C
 * heap) if required. Returns the number of slots obtained.
 *
 * [alloc_idx] is ignored, and must be negative (this interface is
 * also used by the native-code runtime, in which [alloc_idx] is
 * meaningful.
 */

size_t caml_get_callstack(size_t max_slots,
                          backtrace_slot **buffer_p,
                          size_t *alloc_size_p,
                          ptrdiff_t alloc_idx)
{
  CAMLassert(alloc_idx < 1); /* allocation indexes not used in bytecode */
  return get_callstack(Caml_state->current_stack->sp,
                       Caml_state->trap_sp_off,
                       Caml_state->current_stack,
                       max_slots,
                       buffer_p, alloc_size_p);
}

CAMLprim value caml_get_current_callstack (value max_frames_value)
{
  backtrace_slot *backtrace = NULL;
  size_t trace_size = 0;
  size_t slots = get_callstack(Caml_state->current_stack->sp,
                               Caml_state->trap_sp_off,
                               Caml_state->current_stack,
                               Long_val(max_frames_value),
                               &backtrace, &trace_size);
  return alloc_callstack(backtrace, slots);
}

CAMLprim value caml_get_continuation_callstack (value cont, value max_frames)
{
  backtrace_slot *backtrace = NULL;
  size_t trace_size = 0;
  size_t slots;
  struct stack_info *stack;
  value *sp;

  stack = Ptr_val(caml_continuation_use(cont));
  {
    CAMLnoalloc; /* GC must not see the stack outside the cont */
    sp = stack->sp;
    slots = get_callstack(sp, Long_val(sp[0]),
                          stack, Long_val(max_frames),
                          &backtrace, &trace_size);
    caml_continuation_replace(cont, stack);
  }

  return alloc_callstack(backtrace, slots);
}


/* Read the debugging info contained in the current bytecode executable. */

static void read_main_debug_info(struct debug_info *di)
{
  CAMLparam0();
  CAMLlocal3(events, evl, l);
  char_os *exec_name;
  int fd, num_events, orig;
  struct channel *chan;
  struct exec_trailer trail;

  CAMLassert(di->already_read == 0);
  di->already_read = 1;

  /* At the moment, bytecode programs built with --output-complete-exe
     do not contain any debug info.

     See  https://github.com/ocaml/ocaml/issues/9344 for details.
  */
  if (caml_params->cds_file == NULL && caml_byte_program_mode == COMPLETE_EXE)
    CAMLreturn0;

  if (caml_params->cds_file != NULL) {
    exec_name = (char_os*) caml_params->cds_file;
  } else {
    exec_name = (char_os*) caml_params->exe_name;
  }

  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0) {
    /* Record the failure of caml_attempt_open in di->already-read */
    di->already_read = fd;
    CAMLreturn0;
  }

  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") != -1) {
    chan = caml_open_descriptor_in(fd);

    num_events = caml_getword(chan);
    events = caml_alloc(num_events, 0);

    for (int i = 0; i < num_events; i++) {
      orig = caml_getword(chan);
      evl = caml_input_val(chan);
      caml_input_val(chan); /* Skip the list of absolute directory names */
      /* Relocate events in event list */
      for (l = evl; l != Val_int(0); l = Field(l, 1)) {
        value ev = Field(l, 0);
        Store_field (ev, EV_POS, Val_long(Long_val(Field(ev, EV_POS)) + orig));
      }
      /* Record event list */
      Store_field(events, i, evl);
    }

    caml_close_channel(chan);

    di->events = process_debug_events(caml_start_code, events, &di->num_events);
  } else {
    close(fd);
  }

  CAMLreturn0;
}

CAMLexport void caml_init_debug_info(void)
{
  caml_ext_table_init(&caml_debug_info, 1);
  caml_add_debug_info(caml_start_code, Val_long(caml_code_size), Val_unit);
}

CAMLexport void caml_load_main_debug_info(void)
{
  if (caml_params->backtrace_enabled > 1) {
    read_main_debug_info(caml_debug_info.contents[0]);
  }
}

int caml_debug_info_available(void)
{
  return (caml_debug_info.size != 0);
}

int caml_debug_info_status(void)
{
  if (!caml_debug_info_available()) {
    return 0;
  } else {
    return ((struct debug_info *)caml_debug_info.contents[0])->already_read;
  }
}

/* Search the event index for the given PC.  Return -1 if not found. */

static struct ev_info *event_for_location(code_t pc)
{
  uintnat low, high;
  struct debug_info *di = find_debug_info(pc);

  if (di == NULL)
    return NULL;

  if (!di->already_read)
    read_main_debug_info(di);

  if (di->num_events == 0)
    return NULL;

  low = 0;
  high = di->num_events;
  while (low+1 < high) {
    uintnat m = (low+high)/2;
    if(pc < di->events[m].ev_pc) high = m;
    else low = m;
  }
  if (di->events[low].ev_pc == pc)
    return &di->events[low];
  /* ocamlc sometimes moves an event past a following PUSH instruction;
     allow mismatch by 1 instruction. */
  if (di->events[low].ev_pc == pc + 1)
    return &di->events[low];
  if (low+1 < di->num_events && di->events[low+1].ev_pc == pc + 1)
    return &di->events[low+1];

  return NULL;
}

/* Search for event info at the exact given PC. */
struct ev_info * caml_exact_event_for_location(code_t pc)
{
  struct ev_info *ev = event_for_location(pc);
  if (ev && ev->ev_pc == pc)
    return ev;

  return NULL;
}

/* Extract location information for the given PC */

void caml_debuginfo_location(debuginfo dbg,
                             /*out*/ struct caml_loc_info * li)
{
  code_t pc = dbg;
  struct ev_info *event = event_for_location(pc);
  li->loc_is_raise =
    caml_is_instruction(*pc, RAISE) ||
    caml_is_instruction(*pc, RERAISE);
  if (event == NULL) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;
  li->loc_is_inlined = 0;
  li->loc_filename = event->ev_filename;
  li->loc_defname = event->ev_defname;
  li->loc_start_lnum = event->ev_start_lnum;
  li->loc_start_chr = event->ev_start_chr;
  li->loc_end_lnum = event->ev_end_lnum;
  li->loc_end_chr = event->ev_end_chr;
  li->loc_end_offset = event->ev_end_offset;
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  return (debuginfo)slot;
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  /* No inlining in bytecode */
  return NULL;
}
