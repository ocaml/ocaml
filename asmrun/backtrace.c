/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2006 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include "backtrace.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "stack.h"

int caml_backtrace_active = 0;
int caml_backtrace_pos = 0;
code_t * caml_backtrace_buffer = NULL;
value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

/* Initialize the backtrace machinery */

void caml_init_backtrace(void)
{
  caml_backtrace_active = 1;
  caml_register_global_root(&caml_backtrace_last_exn);
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  frame_descr * d;
  uintnat h;

  if (exn != caml_backtrace_last_exn) {
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = exn;
  }
  if (caml_backtrace_buffer == NULL) {
    caml_backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (caml_backtrace_buffer == NULL) return;
  }
  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (1) {
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(pc);
    while(1) {
      d = caml_frame_descriptors[h];
      if (d == 0) return; /* can happen if some code not compiled with -g */
      if (d->retaddr == pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, store its descriptor in the backtrace buffer */
      if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
      caml_backtrace_buffer[caml_backtrace_pos++] = (code_t) d;
#ifndef Stack_grows_upwards
      sp += (d->frame_size & 0xFFFC);
#else
      sp -= (d->frame_size & 0xFFFC);
#endif
      pc = Saved_return_address(sp);
#ifdef Mask_already_scanned
      pc = Mask_already_scanned(pc);
#endif
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      struct caml_context * next_context = Callback_link(sp);
      sp = next_context->bottom_of_stack;
      pc = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
      if (sp == NULL) return;
    }
    /* Stop when we reach the current exception handler */
#ifndef Stack_grows_upwards
    if (sp > trapsp) return;
#else
    if (sp < trapsp) return;
#endif
  }
}

/* Print a backtrace */

static void print_location(int index, frame_descr * d)
{
  uintnat infoptr;
  uint32 info1, info2, k, n, l, a, b;
  char * kind;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to 
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) return;
  /* Recover debugging info */
  infoptr = ((uintnat) d +
             sizeof(char *) + sizeof(short) + sizeof(short) +
             sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
            & -sizeof(frame_descr *);
  info1 = ((uint32 *)infoptr)[0];
  info2 = ((uint32 *)infoptr)[1];
  /* Format of the two info words:
       llllllllllllllllllll aaaaaaaa bbbbbbbbbb nnnnnnnnnnnnnnnnnnnnnnnn kk
                          44       36         26                       2  0 
                       (32+12)    (32+4)
     k ( 2 bits): 0 if it's a call, 1 if it's a raise
     n (24 bits): offset (in 4-byte words) of file name relative to infoptr
     l (20 bits): line number
     a ( 8 bits): beginning of character range
     b (10 bits): end of character range */
  k = info1 & 3;
  n = info1 & 0x3FFFFFC;
  l = info2 >> 12;
  a = (info2 >> 4) & 0xFF;
  b = ((info2 & 0xF) << 6) | (info1 >> 26);

  if (index == 0)
    kind = "Raised at";
  else if (k == 1)
    kind = "Re-raised at";
  else
    kind = "Called from";

  fprintf(stderr, "%s file \"%s\", line %d, characters %d-%d\n",
          kind, ((char *) infoptr) + n, l, a, b);
}

void caml_print_exception_backtrace(void)
{
  int i;

  for (i = 0; i < caml_backtrace_pos; i++)
    print_location(i, (frame_descr *) caml_backtrace_buffer[i]);
}
