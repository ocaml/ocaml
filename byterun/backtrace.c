/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
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

CAMLexport int backtrace_active = 0;
CAMLexport int backtrace_pos = 0;
CAMLexport code_t * backtrace_buffer = NULL;
CAMLexport value backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* Location of fields in the Instruct.debug_event record */
enum { EV_POS = 0,
       EV_MODULE = 1,
       EV_CHAR = 2,
       EV_KIND = 3 };

/* Initialize the backtrace machinery */

void init_backtrace(void)
{
  backtrace_active = 1;
  register_global_root(&backtrace_last_exn);
  /* Note: lazy initialization of backtrace_buffer in stash_backtrace
     to simplify the interface with the thread libraries */
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void stash_backtrace(value exn, code_t pc, value * sp)
{
  code_t end_code = (code_t) ((char *) start_code + code_size);
  if (pc != NULL) pc = pc - 1;
  if (exn != backtrace_last_exn) {
    backtrace_pos = 0;
    backtrace_last_exn = exn;
  }
  if (backtrace_buffer == NULL) {
    backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (backtrace_buffer == NULL) return;
  }
  if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  backtrace_buffer[backtrace_pos++] = pc;
  for (/*nothing*/; sp < trapsp; sp++) {
    code_t p = (code_t) *sp;
    if (p >= start_code && p < end_code) {
      if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      backtrace_buffer[backtrace_pos++] = p;
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

  exec_name = caml_main_argv[0];
  fd = attempt_open(&exec_name, &trail, 1);
  if (fd < 0) CAMLreturn(Val_false);
  read_section_descriptors(fd, &trail);
  if (seek_optional_section(fd, &trail, "DBUG") == -1) {
    close(fd);
    CAMLreturn(Val_false);
  }
  chan = open_descriptor_in(fd);
  num_events = getword(chan);
  events = alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = getword(chan);
    evl = input_val(chan);
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field(l, 1)) {
      value ev = Field(l, 0);
      Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
    }
    /* Record event list */
    Store_field(events, i, evl);
  }
  close_channel(chan);
  CAMLreturn(events);
}

/* Search the event for the given PC.  Return Val_false if not found. */

static value event_for_location(value events, code_t pc)
{
  mlsize_t i;
  value pos, l, ev;

  Assert(pc >= start_code && pc < start_code + code_size);
  pos = Val_long((char *) pc - (char *) start_code);
  for (i = 0; i < Wosize_val(events); i++) {
    for (l = Field(events, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);
      if (Field(ev, EV_POS) == pos /* && Is_block(Field(ev, EV_KIND)) */)
        return ev;
    }
  }
  return Val_false;
}

/* Print the location corresponding to the given PC */

static void print_location(value events, int index)
{
  code_t pc = backtrace_buffer[index];
  char * info;
  value ev;

  if (pc == NULL) {
    fprintf(stderr, "Raised from a C function");
    return;
  }
  ev = event_for_location(events, pc);
  if (is_instruction(*pc, RAISE)) {
    /* Ignore compiler-inserted raise */
    if (ev == Val_false) return;
    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    info = "Called from";
  }
  if (ev == Val_false) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf(stderr, "%s module %s, character %d\n", info,
            String_val(Field(ev, EV_MODULE)),
            Int_val(Field(ev, EV_CHAR)));
  }
}

/* Print a backtrace */

CAMLexport void print_exception_backtrace(void)
{
  value events;
  int i;

  events = read_debug_info();
  if (events == Val_false) {
    fprintf(stderr,
            "(Program not linked with -g, cannot print stack backtrace\n");
    return;
  }
  for (i = 0; i < backtrace_pos; i++)
    print_location(events, i);
}
