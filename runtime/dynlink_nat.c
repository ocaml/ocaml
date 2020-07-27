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
#include "caml/codefrag.h"
#include "caml/alloc.h"
#include "caml/intext.h"
#include "caml/osdeps.h"
#include "caml/fail.h"
#include "caml/signals.h"
#ifdef WITH_SPACETIME
#include "caml/spacetime.h"
#endif

#include "caml/hooks.h"

CAMLexport void (*caml_natdynlink_hook)(void* handle, const char* unit) = NULL;

#include <stdio.h>
#include <string.h>
#include <limits.h>

#define Handle_val(v) (*((void **) Data_abstract_val(v)))
static value Val_handle(void* handle) {
  value res = caml_alloc_small(1, Abstract_tag);
  Handle_val(res) = handle;
  return res;
}

static void *getsym(void *handle, const char *module, const char *name){
  char *fullname = caml_stat_strconcat(3, "caml", module, name);
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
  char_os *p;
  value exn;

  p = caml_stat_strdup_to_os(String_val(filename));
  exn = caml_enter_blocking_section_exn();
  if (Is_exception_result(exn)) goto cleanup1;
  dlhandle = caml_dlopen(p, 1, Int_val(global));
  exn = caml_leave_blocking_section_exn();
  if (Is_exception_result(exn)) goto cleanup2;

  if (NULL == dlhandle) {
    exn = caml_failwith_exn(caml_dlerror());
    goto cleanup2;
  }

  sym = caml_dlsym(dlhandle, "caml_plugin_header");
  if (NULL == sym) {
    exn = caml_failwith_exn("not an OCaml plugin");
    goto cleanup2;
  }

  handle = Val_handle(dlhandle);
  header = caml_input_value_from_block(sym, INT_MAX);

  res = caml_alloc_tuple(2);
  Field(res, 0) = handle;
  Field(res, 1) = header;
  caml_stat_free(p);
  CAMLreturn(res);

 cleanup2:
  if (dlhandle) {
    caml_enter_blocking_section_noexn();
    caml_dlclose(dlhandle);
    caml_leave_blocking_section_noexn();
  }
 cleanup1:
  caml_stat_free(p);
  caml_raise(Extract_exception(exn));
}

CAMLprim value caml_natdynlink_run(value handle_v, value symbol) {
  CAMLparam2 (handle_v, symbol);
  CAMLlocal1 (result);
  void *sym,*sym2;
  void* handle = Handle_val(handle_v);

#define optsym(n) getsym(handle,unit,n)
  const char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (NULL != sym) caml_register_frametable(sym);

#ifdef WITH_SPACETIME
  sym = optsym("__spacetime_shapes");
  if (NULL != sym) caml_spacetime_register_shapes(sym);
#endif

  sym = optsym("__gc_roots");
  if (NULL != sym) caml_register_dyn_global(sym);

  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);

  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (NULL != sym && NULL != sym2)
    caml_register_code_fragment((char *) sym, (char *) sym2,
                                DIGEST_LATER, NULL);

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
  char_os *p;
  value exn;

  p = caml_stat_strdup_to_os(String_val(filename));
  exn = caml_enter_blocking_section_exn();
  if (Is_exception_result(exn)) goto cleanup1;
  handle = caml_dlopen(p, 1, 1);
  exn = caml_leave_blocking_section_exn();
  if (Is_exception_result(exn)) goto cleanup2;
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

 cleanup2:
  if (handle) {
    caml_enter_blocking_section_noexn();
    caml_dlclose(handle);
    caml_leave_blocking_section_noexn();
  }
 cleanup1:
  caml_stat_free(p);
  caml_raise(Extract_exception(exn));
}

CAMLprim value caml_natdynlink_loadsym(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);

  sym = (value) caml_globalsym(String_val(symbol));
  if (!sym) caml_failwith(String_val(symbol));
  CAMLreturn(sym);
}
