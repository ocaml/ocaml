/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Alain Frisch, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/stack.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/intext.h"
#include "caml/osdeps.h"
#include "caml/fail.h"
#include "caml/signals.h"
#include "caml/dynlink.h"

#include <stdio.h>
#include <string.h>
#include <limits.h>

#define Handle_val(v) (*((void **) Data_abstract_val(v)))
static value Val_handle(void* handle) {
  value res = caml_alloc_small(1, Abstract_tag);
  Handle_val(res) = handle;
  return res;
}

CAMLprim void *caml_natdynlink_getsym(void *handle, char *module, char *name){
  char *fullname = caml_strconcat(3, "caml", module, name);
  void *sym;
  sym = caml_dlsym (handle, fullname);
  /*  printf("%s => %lx\n", fullname, (uintnat) sym); */
  caml_stat_free(fullname);
  return sym;
}

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return caml_input_value_from_block(caml_globals_map, INT_MAX);
}

CAMLprim value caml_natdynlink_globals_inited(value unit)
{
  return Val_int(caml_globals_inited);
}

CAMLprim value caml_natdynlink_open(value filename, value global)
{
  CAMLparam2 (filename, global);
  CAMLlocal3 (res, handle, header);
  void *sym;
  void *dlhandle;
  char *p;

  /* TODO: dlclose in case of error... */

  p = caml_strdup(String_val(filename));
  caml_enter_blocking_section();
  dlhandle = caml_dlopen(p, 1, Int_val(global));
  caml_leave_blocking_section();
  caml_stat_free(p);

  if (NULL == dlhandle)
    caml_failwith(caml_dlerror());

  sym = caml_dlsym(dlhandle, "caml_plugin_header");
  if (NULL == sym)
    caml_failwith("not an OCaml plugin");

  handle = Val_handle(dlhandle);
  header = caml_input_value_from_block(sym, INT_MAX);

  res = caml_alloc_tuple(2);
  Field(res, 0) = handle;
  Field(res, 1) = header;
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_run(value handle_v, value symbol) {
  CAMLparam2 (handle_v, symbol);
  CAMLlocal1 (result);
  void *sym,*sym2;
  void* handle = Handle_val(handle_v);
  struct code_fragment * cf;

#define optsym(n) caml_natdynlink_getsym(handle,unit,n)
  char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (NULL != sym) caml_register_frametable(sym);

  sym = optsym("__gc_roots");
  if (NULL != sym) caml_register_dyn_global(sym);

  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);

  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (NULL != sym && NULL != sym2) {
    caml_page_table_add(In_code_area, sym, sym2);
    cf = caml_stat_alloc(sizeof(struct code_fragment));
    cf->code_start = (char *) sym;
    cf->code_end = (char *) sym2;
    cf->digest_computed = 0;
    caml_ext_table_add(&caml_code_fragments_table, cf);
  }

  if( caml_natdynlink_hook != NULL ) caml_natdynlink_hook(handle,unit);

  entrypoint = optsym("__entry");
  if (NULL != entrypoint) result = caml_callback((value)(&entrypoint), 0);
  else result = Val_unit;

#undef optsym

  CAMLreturn (result);
}

CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol)
{
  CAMLparam2 (filename, symbol);
  CAMLlocal3 (res, v, handle_v);
  void *handle;
  char *p;

  /* TODO: dlclose in case of error... */

  p = caml_strdup(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, 1, 1);
  caml_leave_blocking_section();
  caml_stat_free(p);

  if (NULL == handle) {
    res = caml_alloc(1,1);
    v = caml_copy_string(caml_dlerror());
    Store_field(res, 0, v);
  } else {
    handle_v = Val_handle(handle);
    res = caml_alloc(1,0);
    v = caml_natdynlink_run(handle_v, symbol);
    Store_field(res, 0, v);
  }
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_loadsym(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);

  sym = (value) caml_globalsym(String_val(symbol));
  if (!sym) caml_failwith(String_val(symbol));
  CAMLreturn(sym);
}
