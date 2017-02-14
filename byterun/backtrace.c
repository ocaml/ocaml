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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/fail.h"

/* The table of debug information fragments */
struct ext_table caml_debug_info;

CAMLexport int32_t caml_backtrace_active = 0;
CAMLexport int32_t caml_backtrace_pos = 0;
CAMLexport caml_backtrace_item * caml_backtrace_buffer = NULL;
CAMLexport value caml_backtrace_last_exn = Val_unit;

void caml_init_backtrace(void)
{
  caml_register_global_root(&caml_backtrace_last_exn);
}

/* Start or stop the backtrace machinery */
CAMLprim value caml_record_backtrace(value vflag)
{
  int flag = Int_val(vflag);

  if (flag != caml_backtrace_active) {
    caml_backtrace_active = flag;
    caml_backtrace_pos = 0;
    caml_backtrace_last_exn = Val_unit;
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

/* Print location information -- same behavior as in Printexc

   note that the test for compiler-inserted raises is slightly redundant:
     (!li->loc_valid && li->loc_is_raise)
   caml_debuginfo_location guarantees that when li->loc_valid is
   0, then li->loc_is_raise is always 1, so the latter test is
   useless. We kept it to keep code identical to the byterun/
   implementation. */
static void print_location(struct caml_loc_info * li, int index, char* prefix)
{
  char * info;
  char * inlined;

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
  if (li->loc_is_inlined) {
    inlined = " (inlined)";
  } else {
    inlined = "";
  }
  if (! li->loc_valid) {
    fprintf(stderr, "%s%s unknown location%s\n", prefix, info, inlined);
  } else {
    fprintf (stderr, "%s%s file \"%s\"%s, line %d, characters %d-%d\n",
             prefix, info, li->loc_filename, inlined, li->loc_lnum,
             li->loc_startchr, li->loc_endchr);
  }
}

/* [p] can be NULL if the top-of-stack was added after an overflow of
   the buffer size. */
static void print_backtrace_slot(backtrace_slot p, int i, char *prefix)
{
  debuginfo dbg;
  struct caml_loc_info li;

  if( p != NULL ){
    for (dbg = caml_debuginfo_extract(p);
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
      {
        caml_debuginfo_location(dbg, &li);
        print_location(&li, i, prefix);
      }
  } else {
    fprintf(stderr, "long backtrace cut here =========\n");
  }
}

/* Print a backtrace. Only called from printexc.c in the default
   fatal exception handler. */
CAMLexport void caml_print_exception_backtrace(void)
{
  int i;
  backtrace_slot p;
  int count;
  
  if (!caml_debug_info_available()) {
    fprintf(stderr, "(Cannot print stack backtrace: "
                    "no debug information available)\n");
    return;
  }

  /* copy top-of-stack if needed */
  caml_finish_backtrace();
  
  for (i = 0; i < caml_backtrace_pos; i++) {
    p = caml_backtrace_buffer[i].backtrace_descriptor;
    count = caml_backtrace_buffer[i].backtrace_count;
    if( count == 0x10 ){
      print_backtrace_slot(p,i,"");
    } else
      if( (count & 0x01) == 0x01 ){
        fprintf(stderr, "Mutual recursion called %d times:\n", count >> 4);
        print_backtrace_slot(p,i,"  [1] ");
        i++;
        p = caml_backtrace_buffer[i].backtrace_descriptor;
        print_backtrace_slot(p,i,"  [2] ");
      } else {
        fprintf(stderr, "Recursion called %d times:\n", count >> 4);
        print_backtrace_slot(p,i,"  * ");
      }
  }
}

static void save_backtrace_slot(backtrace_slot p, value res, int i)
{
  Field(res, i) = Val_backtrace_slot(p);
}

/* Get a copy of the latest backtrace */
CAMLprim value caml_get_exception_raw_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);

  /* Beware: the allocations below may cause finalizers to be run, and another
     backtrace---possibly of a different length---to be stashed (for example
     if the finalizer raises then catches an exception).  We choose to ignore
     any such finalizer backtraces and return the original one. */

  if (!caml_backtrace_active ||
      caml_backtrace_buffer == NULL ||
      caml_backtrace_pos == 0) {
    res = caml_alloc(0, 0);
  }
  else {
    caml_backtrace_item saved_caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE];
    backtrace_slot p,p2;
    int saved_caml_backtrace_pos;
    int caml_backtrace_size;
    int i, j, pos, count;

    saved_caml_backtrace_pos = caml_backtrace_pos;
    if( caml_backtrace_pos < 0 )
      saved_caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;

    if (saved_caml_backtrace_pos > BACKTRACE_BUFFER_SIZE) {
      saved_caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
    }

    memcpy(saved_caml_backtrace_buffer, caml_backtrace_buffer,
           saved_caml_backtrace_pos * sizeof(caml_backtrace_item));

    /* The size of the uncompressed backtrace can be bigger, we need
       to compute it before the allocation. */
    caml_backtrace_size = 0;
    for (i = 0; i < saved_caml_backtrace_pos; i++){
      count = saved_caml_backtrace_buffer[i].backtrace_count >> 4;
      caml_backtrace_size += count;
    }
    res = caml_alloc(caml_backtrace_size, 0);

    pos = 0;
    i = 0;
    while (pos < caml_backtrace_size) {
      p = saved_caml_backtrace_buffer[i].backtrace_descriptor;
      count = saved_caml_backtrace_buffer[i].backtrace_count;
      i++;
      if( (count & 0x01) == 0x01 ){
        i++;
        p2 = saved_caml_backtrace_buffer[i].backtrace_descriptor;
      }
      if( count == 0x10 ){
        save_backtrace_slot(p, res, pos++);
      } else {
        int n = count >> 4;
        for(j=0; j<n; j++){
          if( (count & 0x01) == 0x01 ){
            save_backtrace_slot(p, res, pos++);
            save_backtrace_slot(p2, res, pos++);
          } else {
            save_backtrace_slot(p, res, pos++);
          }
        }
      }
    }
  }

  CAMLreturn(res);
}

#define Val_debuginfo(bslot) (Val_long((uintnat)(bslot)>>1))
#define Debuginfo_val(vslot) ((debuginfo)(Long_val(vslot) << 1))

/* Convert the raw backtrace to a data structure usable from OCaml */
static value caml_convert_debuginfo(debuginfo dbg)
{
  CAMLparam0();
  CAMLlocal2(p, fname);
  struct caml_loc_info li;

  caml_debuginfo_location(dbg, &li);

  if (li.loc_valid) {
    fname = caml_copy_string(li.loc_filename);
    p = caml_alloc_small(6, 0);
    Field(p, 0) = Val_bool(li.loc_is_raise);
    Field(p, 1) = fname;
    Field(p, 2) = Val_int(li.loc_lnum);
    Field(p, 3) = Val_int(li.loc_startchr);
    Field(p, 4) = Val_int(li.loc_endchr);
    Field(p, 5) = Val_bool(li.loc_is_inlined);
  } else {
    p = caml_alloc_small(1, 1);
    Field(p, 0) = Val_bool(li.loc_is_raise);
  }

  CAMLreturn(p);
}

CAMLprim value caml_convert_raw_backtrace_slot(value slot)
{
  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  return (caml_convert_debuginfo(Debuginfo_val(slot)));
}

/* Convert the raw backtrace to a data structure usable from OCaml */
CAMLprim value caml_convert_raw_backtrace(value bt)
{
  CAMLparam1(bt);
  CAMLlocal1(array);
  intnat i, index;

  if (!caml_debug_info_available())
    caml_failwith("No debug information available");

  for (i = 0, index = 0; i < Wosize_val(bt); ++i)
  {
    debuginfo dbg;
    for (dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
      index++;
  }

  array = caml_alloc(index, 0);

  for (i = 0, index = 0; i < Wosize_val(bt); ++i)
  {
    debuginfo dbg;
    for (dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
         dbg != NULL;
         dbg = caml_debuginfo_next(dbg))
    {
      Store_field(array, index, caml_convert_debuginfo(dbg));
      index++;
    }
  }

  CAMLreturn(array);
}

CAMLprim value caml_raw_backtrace_length(value bt)
{
  return Val_int(Wosize_val(bt));
}

CAMLprim value caml_raw_backtrace_slot(value bt, value index)
{
  uintnat i;
  debuginfo dbg;

  i = Long_val(index);
  if (i >= Wosize_val(bt))
    caml_invalid_argument("Printexc.get_raw_backtrace_slot: "
                          "index out of bounds");
  dbg = caml_debuginfo_extract(Backtrace_slot_val(Field(bt, i)));
  return Val_debuginfo(dbg);
}

CAMLprim value caml_raw_backtrace_next_slot(value slot)
{
  debuginfo dbg;

  CAMLparam1(slot);
  CAMLlocal1(v);

  dbg = Debuginfo_val(slot);
  dbg = caml_debuginfo_next(dbg);

  if (dbg == NULL)
    v = Val_int(0); /* None */
  else
  {
    v = caml_alloc(1, 0);
    Field(v, 0) = Val_debuginfo(dbg);
  }

  CAMLreturn(v);
}

/* the function below is deprecated: we previously returned directly
   the OCaml-usable representation, instead of the raw backtrace as an
   abstract type, but this has a large performance overhead if you
   store a lot of backtraces and print only some of them.

   It is not used by the Printexc library anymore, or anywhere else in
   the compiler, but we have kept it in case some user still depends
   on it as an external.  */
CAMLprim value caml_get_exception_backtrace(value unit)
{
  CAMLparam0();
  CAMLlocal3(arr, res, backtrace);
  intnat i;

  if (!caml_debug_info_available()) {
    res = Val_int(0); /* None */
  } else {
    backtrace = caml_get_exception_raw_backtrace(Val_unit);

    arr = caml_alloc(Wosize_val(backtrace), 0);
    for (i = 0; i < Wosize_val(backtrace); i++) {
      backtrace_slot slot = Backtrace_slot_val(Field(backtrace, i));
      debuginfo dbg = caml_debuginfo_extract(slot);
      Store_field(arr, i, caml_convert_debuginfo(dbg));
    }

    res = caml_alloc_small(1, 0); Field(res, 0) = arr; /* Some */
  }

  CAMLreturn(res);
}

/* When [caml_backtrace_pos] reaches [BACKTRACE_BUFFER_SIZE], we have
normally no space to record the top of the backtrace. Here, we create
a space with [TOP_OF_BACKTRACE] entries, called
[top_of_backtrace]. [caml_backtrace_pos] is set to a negative
value. Descriptors are stored in this *cyclic* buffer. When leaving
[caml_stash_backtrace], we call [caml_finish_backtrace] to copy
[top_of_backtrace] to the standard backtrace buffer, taking only the
[TOP_OF_BACKTRACE_SIZE] top entries. We add a NULL pointer as a frame
descriptor, that is printed as "<<< backtrace cut here >>>".
 */
#define TOP_OF_BACKTRACE_SIZE 64
static backtrace_slot top_of_backtrace[TOP_OF_BACKTRACE_SIZE];
CAMLexport void caml_finish_backtrace()
{
  if(caml_backtrace_pos < 0){
    int i;
    caml_backtrace_item* p = &caml_backtrace_buffer[BACKTRACE_BUFFER_SIZE-1];
    caml_backtrace_pos++;
    for(i=0; i<TOP_OF_BACKTRACE_SIZE && caml_backtrace_pos < 0; i++){
      backtrace_slot d = 
        top_of_backtrace[ (1-caml_backtrace_pos) % TOP_OF_BACKTRACE_SIZE ];
      caml_backtrace_pos++;
      p->backtrace_descriptor = d;
      p->backtrace_count = 0x10;
      p--;
    }
    if( (p->backtrace_count & 0x01) &&
        (p[-1].backtrace_count & 0x01) ){
      p[-1].backtrace_count = 0x10;
      p[-1].backtrace_descriptor = NULL;
    }
    p->backtrace_descriptor = NULL;
    p->backtrace_count = 0x10;
    caml_backtrace_pos = BACKTRACE_BUFFER_SIZE;
  }
}

static void really_store_backtrace_item(backtrace_slot d)
{
  if (caml_backtrace_pos >= BACKTRACE_BUFFER_SIZE){
    caml_backtrace_pos = -1;
  }
  if(caml_backtrace_pos < 0){
    top_of_backtrace[ (1-caml_backtrace_pos) % TOP_OF_BACKTRACE_SIZE ] = d;
    caml_backtrace_pos--;
  } else {
    caml_backtrace_item *p = &caml_backtrace_buffer[caml_backtrace_pos++];
    p -> backtrace_descriptor = d;
    p -> backtrace_count = 0x10;
  }
}

CAMLexport void caml_store_backtrace_slot(backtrace_slot d)
{
  caml_backtrace_item *prev1, *prev2, *prev3;
  if( caml_backtrace_pos >= 4 ){

    /* Detect cycle of size 1 */
    prev1 = &caml_backtrace_buffer[caml_backtrace_pos-1];
    if ( prev1->backtrace_descriptor == d &&
         (prev1->backtrace_count & 0xf) == 0 ){
      prev1->backtrace_count += 0x10;
      return;
    }

    /* Detect cycle of size 2 */
    prev2 = &caml_backtrace_buffer[caml_backtrace_pos-2];
    prev3 = &caml_backtrace_buffer[caml_backtrace_pos-3];
    if(
       (prev2->backtrace_descriptor == d) &&
       (prev1->backtrace_descriptor == prev3->backtrace_descriptor) &&
       (prev2->backtrace_count == prev3->backtrace_count) &&
       (prev1->backtrace_count == 0x10) &&
       ( (prev2->backtrace_count & 0x01) == 0x01
         || prev2->backtrace_count == 0x10)
       ) {
      caml_backtrace_pos--;
      prev2->backtrace_count = (prev2->backtrace_count + 0x10) | 0x01;
      prev3->backtrace_count = prev2->backtrace_count;
      return;
    }
  }
  really_store_backtrace_item(d);
}
