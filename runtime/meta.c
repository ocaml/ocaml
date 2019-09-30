/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Primitives for the toplevel */

#include <string.h>
#include "caml/alloc.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/interp.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/stacks.h"
#include "caml/debugger.h"
#include "caml/backtrace_prim.h"

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

struct bytecode {
  code_t prog;
  asize_t len;
};
#define Bytecode_val(p) ((struct bytecode*)Data_abstract_val(p))

/* Convert a bytes array (= LongString.t) to a contiguous buffer.
   The result is allocated with caml_stat_alloc */
static char* buffer_of_bytes_array(value ls, asize_t *len)
{
  CAMLparam1(ls);
  CAMLlocal1(s);
  asize_t off;
  char *ret;
  int i;

  *len = 0;
  for (i = 0; i < Wosize_val(ls); i++) {
    s = Field(ls, i);
    *len += caml_string_length(s);
  }

  ret = caml_stat_alloc(*len);
  off = 0;
  for (i = 0; i < Wosize_val(ls); i++) {
    size_t s_len;
    s = Field(ls, i);
    s_len = caml_string_length(s);
    memcpy(ret + off, Bytes_val(s), s_len);
    off += s_len;
  }

  CAMLreturnT (char*, ret);
}

CAMLprim value caml_reify_bytecode(value ls_prog,
                                   value debuginfo,
                                   value digest_opt)
{
  CAMLparam3(ls_prog, debuginfo, digest_opt);
  CAMLlocal3(clos, bytecode, retval);
  struct code_fragment * cf = caml_stat_alloc(sizeof(struct code_fragment));
  code_t prog;
  asize_t len;

  prog = (code_t)buffer_of_bytes_array(ls_prog, &len);
  caml_add_debug_info(prog, Val_long(len), debuginfo);
  cf->code_start = (char *) prog;
  cf->code_end = (char *) prog + len;
  /* match (digest_opt : string option) with */
  if (Is_block(digest_opt)) {
    /* | Some digest -> */
    memcpy(cf->digest, String_val(Field(digest_opt, 0)), 16);
    cf->digest_computed = 1;
  } else {
    /* | None -> */
    cf->digest_computed = 0;
  }
  caml_ext_table_add(&caml_code_fragments_table, cf);

#ifdef ARCH_BIG_ENDIAN
  caml_fixup_endianness((code_t) prog, len);
#endif
#ifdef THREADED_CODE
  caml_thread_code((code_t) prog, len);
#endif
  caml_prepare_bytecode((code_t) prog, len);

  /* Notify debugger after fragment gets added and reified. */
  caml_debugger(CODE_LOADED, Val_long(caml_code_fragments_table.size - 1));

  clos = caml_alloc_small (1, Closure_tag);
  Code_val(clos) = (code_t) prog;
  bytecode = caml_alloc_small (2, Abstract_tag);
  Bytecode_val(bytecode)->prog = prog;
  Bytecode_val(bytecode)->len = len;
  retval = caml_alloc_small (2, 0);
  Field(retval, 0) = bytecode;
  Field(retval, 1) = clos;
  CAMLreturn (retval);
}

/* signal to the interpreter machinery that a bytecode is no more
   needed (before freeing it) - this might be useful for a JIT
   implementation */

CAMLprim value caml_static_release_bytecode(value bc)
{
  code_t prog;
  asize_t len;
  int found, index;
  struct code_fragment *cf;

  prog = Bytecode_val(bc)->prog;
  len = Bytecode_val(bc)->len;
  caml_remove_debug_info(prog);

  found = caml_find_code_fragment((char*) prog, &index, &cf);
  /* Not matched with a caml_reify_bytecode call; impossible. */
  CAMLassert(found); (void) found; /* Silence unused variable warning. */

  /* Notify debugger before the fragment gets destroyed. */
  caml_debugger(CODE_UNLOADED, Val_long(index));

  caml_ext_table_remove(&caml_code_fragments_table, cf);

#ifndef NATIVE_CODE
  caml_release_bytecode(prog, len);
#else
  caml_failwith("Meta.static_release_bytecode impossible with native code");
#endif
  caml_stat_free(prog);
  return Val_unit;
}

CAMLprim value caml_realloc_global(value size)
{
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = Long_val(size);
  actual_size = Wosize_val(caml_global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    caml_gc_message (0x08, "Growing global data to %"
                     ARCH_INTNAT_PRINTF_FORMAT "u entries\n",
                     requested_size);
    new_global_data = caml_alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      caml_initialize(&Field(new_global_data, i), Field(caml_global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = Val_long (0);
    }
    caml_global_data = caml_check_urgent_gc(new_global_data);
  }
  return Val_unit;
}

CAMLprim value caml_get_current_environment(value unit)
{
  return *Caml_state->extern_sp;
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

  osp = Caml_state->extern_sp;
  Caml_state->extern_sp -= 4;
  nsp = Caml_state->extern_sp;
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

value caml_reify_bytecode(value prog, value len)
{
  caml_invalid_argument("Meta.reify_bytecode");
  return Val_unit; /* not reached */
}

value caml_static_release_bytecode(value prog, value len)
{
  caml_invalid_argument("Meta.static_release_bytecode");
  return Val_unit; /* not reached */
}

void (* volatile caml_async_action_hook)(void);

#endif
