/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Primitives for the toplevel */

#include "alloc.h"
#include "config.h"
#include "fail.h"
#include "fix_code.h"
#include "interp.h"
#include "intext.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "stacks.h"

#ifndef NATIVE_CODE

CAMLprim value caml_get_global_data(value unit)
{
  return caml_global_data;
}

char * caml_section_table = NULL;
asize_t caml_section_table_size;

CAMLprim value caml_get_section_table(value unit)
{
  if (caml_section_table == NULL) caml_raise_not_found();
  return caml_input_value_from_block(caml_section_table,
                                     caml_section_table_size);
}

CAMLprim value caml_reify_bytecode(value prog, value len)
{
  value clos;
#ifdef ARCH_BIG_ENDIAN
  caml_fixup_endianness((code_t) prog, (asize_t) Long_val(len));
#endif
#ifdef THREADED_CODE
  caml_thread_code((code_t) prog, (asize_t) Long_val(len));
#endif
  caml_prepare_bytecode((code_t) prog, (asize_t) Long_val(len));
  clos = caml_alloc_small (1, Closure_tag);
  Code_val(clos) = (code_t) prog;
  return clos;
}

CAMLprim value caml_realloc_global(value size)
{
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = Long_val(size);
  actual_size = Wosize_val(caml_global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    caml_gc_message (0x08, "Growing global data to %lu entries\n",
                     requested_size);
    new_global_data = caml_alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      caml_initialize(&Field(new_global_data, i), Field(caml_global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = Val_long (0);
    }
    caml_global_data = new_global_data;
  }
  return Val_unit;
}
    
CAMLprim value caml_get_current_environment(value unit)
{
  return *caml_extern_sp;
}

CAMLprim value caml_invoke_traced_function(value codeptr, value env, value arg)
{
  /* Stack layout on entry:
       return frame into instrument_closure function
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       arg1 to call_original_code (codeptr)
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  /* Stack layout on exit:
       return frame into instrument_closure function
       actual arg to code (arg)
       pseudo return frame into codeptr:
         extra_args = 0
         environment = env
         PC = codeptr
       arg3 to call_original_code (arg)                   same 6 bottom words as
       arg2 to call_original_code (env)                   on entrance, but
       arg1 to call_original_code (codeptr)               shifted down 4 words
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  value * osp, * nsp;
  int i;

  osp = caml_extern_sp;
  caml_extern_sp -= 4;
  nsp = caml_extern_sp;
  for (i = 0; i < 6; i++) nsp[i] = osp[i];
  nsp[6] = codeptr;
  nsp[7] = env;
  nsp[8] = Val_int(0);
  nsp[9] = arg;
  return Val_unit;
}

#else

/* Dummy definitions to support compilation of ocamlc.opt */

value caml_get_global_data(value unit)
{
  caml_invalid_argument("Meta.get_global_data");
  return Val_unit; /* not reached */
}

value caml_get_section_table(value unit)
{
  caml_invalid_argument("Meta.get_section_table");
  return Val_unit; /* not reached */
}

value caml_realloc_global(value size)
{
  caml_invalid_argument("Meta.realloc_global");
  return Val_unit; /* not reached */
}
    
value caml_invoke_traced_function(value codeptr, value env, value arg)
{
  caml_invalid_argument("Meta.invoke_traced_function");
  return Val_unit; /* not reached */
}

value * caml_stack_low;
value * caml_stack_high;
value * caml_stack_threshold;
value * caml_extern_sp;
value * caml_trapsp;
int caml_callback_depth;
int volatile caml_something_to_do;
void (* volatile caml_async_action_hook)(void);
struct longjmp_buffer * caml_external_raise;

#endif
