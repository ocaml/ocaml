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
#include "misc.h"

/* Need to make a difference native/bytecode, since
   closures (sync channels) have different layouts */

CAMLprim value caml_is_bytecode(value unit) {
  CAMLparam1(unit) ;
#ifdef NATIVE_CODE
  CAMLreturn(Val_bool(0)) ;
#else
  CAMLreturn (Val_bool(1)) ;
#endif
}

static int join_marshal_initialized = 0 ;

#define INITIAL_SAVED_POINTERS_SIZE 64
static value *saved_pointers = (value)NULL;
static mlsize_t saved_pointers_size;
static mlsize_t nbr_saved_pointers;


static void (*prev_scan_roots_hook) (scanning_action);

static void marshal_scan_roots(scanning_action action)
{
  value *sp = saved_pointers;
  if (sp) {
    mlsize_t i;
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
  mlsize_t i;

  saved_pointers_size = INITIAL_SAVED_POINTERS_SIZE;
  saved_pointers = (value *)caml_stat_alloc(saved_pointers_size*sizeof(value));
  for (i = 0; i < saved_pointers_size; i++) 
    Field(saved_pointers,i) = Val_int(0);
}

/* internal use */
static void resize_saved_pointers(void)
{
  mlsize_t i;

  saved_pointers_size *= 2;
  saved_pointers = (value *)caml_stat_resize((void *)saved_pointers,
			       saved_pointers_size*sizeof(value));
  for(i=nbr_saved_pointers;i<saved_pointers_size;++i)
    Field(saved_pointers,i) = Val_int(0);
}

/* Called for copying a value to the saved pointers block */
static mlsize_t reloc_pointer(value v)
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
      caml_stat_free((void *)saved_pointers);
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


CAMLprim value caml_globalize_message(value v, value flags) {
  CAMLparam2(v, flags) ;
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
    mlsize_t i = nbr_saved_pointers;
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
  CAMLreturn (res) ;
}

static value used_pointers = Val_unit ;
static int unmarshal_initialized = 0 ;

CAMLprim value caml_localize_message(value buff, value pointers) {
  CAMLparam2(buff, pointers) ;
  CAMLlocal1(res) ;

  if (!unmarshal_initialized) {
    caml_register_global_root(&used_pointers) ;
    unmarshal_initialized = 1 ;
  }

  used_pointers = pointers ;
  inside_join = 1 ;

  res = caml_input_val_from_string (buff, 0) ;

  inside_join = 0 ;
  used_pointers = Val_unit ;

  CAMLreturn (res) ;
}

static void stub_serialize(value v,
                           uintnat * wsize_32,
                           uintnat * wsize_64)
{
  mlsize_t pos = 0 ;
  if (!inside_join) {
    free_saved_pointers() ;
    extern_invalid_argument("output_value: stub_value");
  }
  pos = reloc_pointer(v) ;
  caml_serialize_int_4(pos);
  /* 3 values */
  *wsize_32 = 3 * 4 ;
  *wsize_64 = 3 * 8 ;
}

static uintnat stub_deserialize(void * dst) {
  if (inside_join) {
    mlsize_t pos = caml_deserialize_uint_4();
    value *p = dst ;
    value *src = (value *)Field(used_pointers, pos) ;
    /* Only item at index 1 can be a block, cf. alloc_sub below */
    p[0] = src[0] ;
    caml_initialize(p+1,src[1]) ;
    p[2] = src[2] ;
  } else {
    caml_intern_cleanup();
    caml_failwith("input_value: stub value") ;
  }
  return 3*sizeof(value) ;
}

static struct custom_operations join_stub_ops = {
  "_a",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  stub_serialize,
  stub_deserialize,
};


/* Allocate automaton structure, cf. the stub type type in join_types.mli */


CAMLprim value caml_alloc_stub(value a)
{
  CAMLparam1 (a) ;
  CAMLlocal1 (res) ;
  res = caml_alloc_shr(4, JoCustom_tag) ;
  Field(res, 0) = (value)&join_stub_ops ;
  Field(res, 1) = Val_int(0) ; /* for constructor Local */
  caml_initialize(&Field(res, 2),a) ;
  Field(res, 3) = Val_int(0) ; /* initial uid <-> not exported yet */
  CAMLreturn (res) ;
}

CAMLprim value caml_init_join(value unit) {
  CAMLparam1(unit) ;
  caml_register_custom_operations(&join_stub_ops);
  CAMLreturn(Val_unit) ;
}
