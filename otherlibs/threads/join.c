/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Luc Maranget, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2005 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <stdlib.h>
#include <stdio.h>

#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "roots.h"
#include "custom.h"
#include "intext.h"
#include "fail.h"


static int join_marshal_initialized = 0 ;

#define INITIAL_SAVED_POINTERS_SIZE 64
static value *saved_pointers = (value)NULL;
static int32 saved_pointers_size;
static int32 nbr_saved_pointers;


static void (*prev_scan_roots_hook) (scanning_action);

static void marshal_scan_roots(scanning_action action)
{
  value *sp = saved_pointers;
  if (sp) {
    int32 i;
    for(i=0;i<nbr_saved_pointers;i++){
      action(*sp,sp);
      sp++;
    }
  }
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}


/**************************************/
/* External block of 'saved pointers' */
/**************************************/

/* internal use */
static void alloc_saved_pointers(void)
{
  int32 i;

  saved_pointers_size = INITIAL_SAVED_POINTERS_SIZE;
  saved_pointers = (value *)caml_stat_alloc(saved_pointers_size*sizeof(value));
  for (i = 0; i < saved_pointers_size; i++) 
    Field(saved_pointers,i) = Val_int(0);
}

/* internal use */
static void resize_saved_pointers(void)
{
  int32 i;

  saved_pointers_size *= 2;
  saved_pointers = (value *)caml_stat_resize((char*)saved_pointers,
			       saved_pointers_size*sizeof(value));
  for(i=nbr_saved_pointers;i<saved_pointers_size;++i)
    Field(saved_pointers,i) = Val_int(0);
}

/* Called for copying a value to the saved pointers block */
static int32 reloc_pointer(value v)
{
  if(saved_pointers==(value)NULL) 
    alloc_saved_pointers();
  else
    if(nbr_saved_pointers==saved_pointers_size)
      resize_saved_pointers();
    Field(saved_pointers,nbr_saved_pointers) = v;
  nbr_saved_pointers++;
  return nbr_saved_pointers-1 ;
}

/* Free saved pointers space, only when greater then initial size */
static void free_saved_pointers(void)
{
  if (saved_pointers != (value)NULL){
    if (saved_pointers_size > INITIAL_SAVED_POINTERS_SIZE){
      caml_stat_free((char *) saved_pointers);
      saved_pointers = (value)NULL;
    } else {
      while(nbr_saved_pointers){
	nbr_saved_pointers--;
	Field(saved_pointers,nbr_saved_pointers) = Val_int(0);
      }
    }
  }
}

/* Entry point for marshalling messages */
static int inside_join = 0 ;


CAMLprim value caml_marshal_message(value v, value flags) {
  CAMLlocal3(str,array,res) ;
    
  if (!join_marshal_initialized) {
    prev_scan_roots_hook = caml_scan_roots_hook;
    caml_scan_roots_hook = marshal_scan_roots;
    join_marshal_initialized = 1 ;
  }

  nbr_saved_pointers = 0;

  /* Marshal v into string str */
  inside_join = 1 ;
  str = caml_output_value_to_string(v, flags) ;
  inside_join = 0 ;

  /* Copy saved pointers to heap array */
  if(nbr_saved_pointers) {
    int32 i = nbr_saved_pointers;
    array = caml_alloc(nbr_saved_pointers,0);
    while(i>0){
      i--;
      Store_field(array,i,Field(saved_pointers,i)) ;
    }
    free_saved_pointers();
  } else {
    array = Atom(0);
  }

  /* Finally return pair */
  res = caml_alloc_small(2, 0) ;
  Field(res,0)=str;
  Field(res,1)=array;
  return res ;
}

static value used_stubs = Val_unit ;
static int unmarshal_initialized = 0 ;

CAMLprim value caml_unmarshal_message(value buff, value stubs) {
  CAMLparam2(buff, stubs) ;
  CAMLlocal1(res) ;

  if (!unmarshal_initialized) {
    caml_register_global_root(&used_stubs) ;
    unmarshal_initialized = 1 ;
  }

  used_stubs = stubs ;
  inside_join = 1 ;

  res = caml_input_val_from_string (buff, 0) ;

  inside_join = 0 ;
  used_stubs = Val_unit ;

  CAMLreturn (res) ;
}

static void stub_serialize(value v,
                           unsigned long * wsize_32,
                           unsigned long * wsize_64)
{
  int32 pos = 0 ;
  if (!inside_join) {
    free_saved_pointers() ;
    extern_invalid_argument("output_value: stub_value");
  }
  pos = reloc_pointer(v) ;
  fprintf(stderr, "Reloc -> %i\n", pos) ;
  caml_serialize_int_4(pos);
  *wsize_32 = *wsize_64 = 4 ;
}

static unsigned long stub_deserialize(void * dst) {
  if (inside_join) {
    int32 pos = caml_deserialize_uint_4();
    fprintf(stderr, "Found: %i\n", pos) ;
    ((value *)dst)[0] = Field(used_stubs, pos) ;
  } else {
    caml_intern_cleanup();
    caml_failwith("input_value: stub value") ;
  }
  return sizeof(value) ;
}

static struct custom_operations join_stub_ops = {
  "_a",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  stub_serialize,
  stub_deserialize
};


/* Allocate automaton structure, cf. the stub type type in join_types.ml */


CAMLprim value caml_alloc_stub(value a)
{
  CAMLparam1 (a) ;
  CAMLlocal1 (res) ;
  res = caml_alloc_small(2, JoCustom_tag) ;
  Field(res, 0) = (value)&join_stub_ops ;
  Field(res, 1) = a ;
  CAMLreturn (res) ;
}

CAMLprim value caml_init_join(value unit) {
  CAMLparam1(unit) ;
  caml_register_custom_operations(&join_stub_ops);
  CAMLreturn(Val_unit) ;
}
