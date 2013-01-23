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
#include "alloc.h"
#include "backtrace.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "stack.h"

int caml_backtrace_active = 0;
int caml_backtrace_pos = 0;
int caml_backtrace_extract = 0;
code_t * caml_backtrace_buffer = NULL;
value caml_backtrace_last_exn = Val_unit;
#define BACKTRACE_BUFFER_SIZE 1024

CAMLprim value caml_set_backtrace_extract(value vflag){
  caml_backtrace_extract = Int_val(vflag);
  return Val_unit;
}

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
    if (sp > trapsp && !caml_backtrace_extract) return;
#else
    if (sp < trapsp && !caml_backtrace_extract) return;
#endif
  }
}

/* Extract location information for the given frame descriptor */

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

static void extract_location_info(frame_descr * d,
                                  /*out*/ struct loc_info * li)
{
  uintnat infoptr;
  uint32 info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if ((d->frame_size & 1) == 0) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    return;
  }
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
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 3) != 0;
  li->loc_filename = (char *) infoptr + (info1 & 0x3FFFFFC);
  li->loc_lnum = info2 >> 12;
  li->loc_startchr = (info2 >> 4) & 0xFF;
  li->loc_endchr = ((info2 & 0xF) << 6) | (info1 >> 26);
}

static void print_location(struct loc_info * li, int index)
{
  char * info;

  /* Ignore compiler-inserted raise */
  if (!li->loc_valid) return;

  if (index == 0)
    info = "Raised at";
  else if (li->loc_is_raise)
    info = "Re-raised at";
  else
    info = "Called from";
  fprintf (stderr, "%s file \"%s\", line %d, characters %d-%d\n",
           info, li->loc_filename, li->loc_lnum,
           li->loc_startchr, li->loc_endchr);
}

/* Print a backtrace */

void caml_print_exception_backtrace(void)
{
  int i;
  struct loc_info li;

  for (i = 0; i < caml_backtrace_pos; i++) {
    extract_location_info((frame_descr *) (caml_backtrace_buffer[i]), &li);
    print_location(&li, i);
  }
}

/* Convert the backtrace to a data structure usable from OCaml */

CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal4(res, arr, p, fname);
  int i;
  struct loc_info li;

  arr = caml_alloc(caml_backtrace_pos, 0);
  for (i = 0; i < caml_backtrace_pos; i++) {
    extract_location_info((frame_descr *) (caml_backtrace_buffer[i]), &li);
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
  CAMLreturn(res);
}
