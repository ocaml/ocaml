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
#include "caml/frame_descriptors.h"
#include "caml/globroots.h"
#include "caml/signals.h"

#include "caml/hooks.h"

intnat caml_globals_inited = 0;

CAMLexport void (*caml_natdynlink_hook)(void* handle, const char* unit) = NULL;

#include <stdio.h>
#include <string.h>
#include <limits.h>

#define CAML_SYM_SEPARATOR "$"

#define Handle_val(v) (*((void **) Data_abstract_val(v)))
static value Val_handle(void* handle) {
  value res = caml_alloc_small(1, Abstract_tag);
  Handle_val(res) = handle;
  return res;
}

static void *getsym(void *handle, const char *module, const char *name){
  char *fullname;
  fullname = caml_stat_strconcat(4, "caml", module, CAML_SYM_SEPARATOR, name);
  void *sym;
  sym = caml_dlsym (handle, fullname);
  /*  printf("%s => %lx\n", fullname, (uintnat) sym); */
  caml_stat_free(fullname);
  return sym;
}

extern char caml_globals_map[];

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
  const void *sym;
  void *dlhandle;
  char_os *p;
  int global_dup;

  /* TODO: dlclose in case of error... */

  p = caml_stat_strdup_to_os(String_val(filename));
  global_dup = Int_val(global);
  caml_enter_blocking_section();
  dlhandle = caml_dlopen(p, global_dup);
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

CAMLprim value caml_natdynlink_register(value handle_v, value symbols) {
  CAMLparam2 (handle_v, symbols);
  int nsymbols = Wosize_val(symbols);
  void* handle = Handle_val(handle_v);
  void** table;

  table = caml_stat_alloc(sizeof(void*) * nsymbols);

  for (int i = 0; i < nsymbols; i++) {
    const char* unit = String_val(Field(symbols, i));
    table[i] = getsym(handle, unit, "frametable");
    if (table[i] == NULL) {
      caml_stat_free(table);
      caml_invalid_argument_value(
        caml_alloc_sprintf("Dynlink: Missing frametable for %s", unit));
    }
  }
  caml_register_frametables(table, nsymbols);

  for (int i = 0; i < nsymbols; i++) {
    const char* unit = String_val(Field(symbols, i));
    table[i] = getsym(handle, unit, "gc_roots");
    if (table[i] == NULL) {
      caml_stat_free(table);
      caml_invalid_argument_value(
        caml_alloc_sprintf("Dynlink: Missing gc_roots for %s", unit));
    }
  }
  caml_register_dyn_globals(table, nsymbols);

  for (int i = 0; i < nsymbols; i++) {
    const char* unit = String_val(Field(symbols, i));
    void* sym = getsym(handle, unit, "code_begin");
    void* sym2 = getsym(handle, unit, "code_end");
    /* Do not register empty code fragments */
    if (NULL != sym && NULL != sym2 && sym != sym2) {
      caml_register_code_fragment((char *) sym, (char *) sym2,
                                  DIGEST_LATER, NULL);
    }
  }

  caml_stat_free(table);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_natdynlink_run(value handle_v, value symbol) {
  CAMLparam2 (handle_v, symbol);
  CAMLlocal1 (result);
  void* handle = Handle_val(handle_v);
  const char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  if( caml_natdynlink_hook != NULL ) caml_natdynlink_hook(handle,unit);

  entrypoint = getsym(handle, unit, "entry");
  if (NULL != entrypoint) result = caml_callback((value)(&entrypoint), 0);
  else result = Val_unit;

  CAMLreturn (result);
}

CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol)
{
  CAMLparam2 (filename, symbol);
  CAMLlocal4 (res, v, handle_v, symbols);
  void *handle;
  char_os *p;

  /* TODO: dlclose in case of error... */

  p = caml_stat_strdup_to_os(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, 1);
  caml_leave_blocking_section();
  caml_stat_free(p);

  if (NULL == handle) {
    res = caml_alloc(1,1);
    v = caml_copy_string(caml_dlerror());
    Store_field(res, 0, v);
  } else {
    handle_v = Val_handle(handle);

    symbols = caml_alloc_small(1, 0);
    Field(symbols, 0) = symbol;
    (void) caml_natdynlink_register(handle_v, symbols);

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
