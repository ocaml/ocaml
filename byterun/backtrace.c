/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
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
#include "startup.h"
#include "stacks.h"
#include "sys.h"
#include "backtrace.h"

CAMLexport int caml_backtrace_active = 0;
CAMLexport int caml_backtrace_pos = 0;
CAMLexport code_t * caml_backtrace_buffer = NULL;
CAMLexport value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* Location of fields in the Instruct.debug_event record */
enum { EV_POS = 0,
       EV_MODULE = 1,
       EV_CHAR = 2,
       EV_KIND = 3 };

/* Location of fields in the Lexing.position record. */
enum {
  POS_FNAME = 0,
  POS_LNUM = 1,
  POS_BOL = 2,
  POS_CNUM = 3
};

/* Initialize the backtrace machinery */

void caml_init_backtrace(void)
{
  caml_backtrace_active = 1;
  caml_register_global_root(&caml_backtrace_last_exn);
  /* Note: lazy initialization of caml_backtrace_buffer in caml_stash_backtrace
     to simplify the interface with the thread libraries */
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

/* Read the debugging info contained in the current bytecode executable.
   Return a Caml array of Caml lists of debug_event records in "events",
   or Val_false on failure. */

#ifndef O_BINARY
#define O_BINARY 0
#endif

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

  exec_name = caml_exe_name;
  fd = caml_attempt_open(&exec_name, &trail, 1);
  if (fd < 0) CAMLreturn(Val_false);
  caml_read_section_descriptors(fd, &trail);
  if (caml_seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
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
  value pos, l, ev, ev_pos;

  Assert(pc >= caml_start_code && pc < caml_start_code + caml_code_size);
  pos = Val_long((char *) pc - (char *) caml_start_code);
  for (i = 0; i < Wosize_val(events); i++) {
    for (l = Field(events, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);
      ev_pos = Field(ev, EV_POS);
      /* ocamlc sometimes moves an event past a following PUSH instruction;
         allow mismatch by 1 instruction. */
      if (ev_pos == pos || ev_pos == pos + 8) return ev;
    }
  }
  return Val_false;
}

/* Print the location corresponding to the given PC */

static void print_location(value events, int index)
{
  code_t pc = caml_backtrace_buffer[index];
  char * info;
  value ev;

  ev = event_for_location(events, pc);
  if (caml_is_instruction(*pc, RAISE)) {
    /* Ignore compiler-inserted raise */
    if (ev == Val_false) return;
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
  if (ev == Val_false) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    value ev_char = Field (ev, EV_CHAR);
    char *fname = String_val (Field (ev_char, POS_FNAME));
    int lnum = Int_val (Field (ev_char, POS_LNUM));
    int chr = Int_val (Field (ev_char, POS_CNUM))
              - Int_val (Field (ev_char, POS_BOL));
    fprintf (stderr, "%s file \"%s\", line %d, character %d\n", info, fname,
             lnum, chr);
  }
}

/* Print a backtrace */

CAMLexport void caml_print_exception_backtrace(void)
{
  value events;
  int i;

  events = read_debug_info();
  if (events == Val_false) {
    fprintf(stderr,
            "(Program not linked with -g, cannot print stack backtrace)\n");
    return;
  }
  for (i = 0; i < caml_backtrace_pos; i++)
    print_location(events, i);
}
