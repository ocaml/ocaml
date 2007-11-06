#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"
#include "alloc.h"
#include "natdynlink.h"
#include "osdeps.h"
#include "fail.h"

#include <stdio.h>
#include <string.h>

static void *getsym(void *handle, char *module, char *name, int opt){  	 
  char *fullname = malloc(strlen(module) + strlen(name) + 5); 	 
  void *sym; 	 
  sprintf(fullname, "caml%s%s", module, name); 	 
  sym = caml_dlsym (handle, fullname); 	 
  /*  printf("%s => %lx\n", fullname, (uintnat) sym); */
  free(fullname); 	 
  if (NULL == sym && !opt) { 	 
    printf("natdynlink: cannot find symbol %s\n", fullname); 	 
    exit(2); 	 
  } 	 
  return sym; 	 
} 	 
 

/* Data segments are used by the Is_atom predicate (mlvalues.h)
   to detect static Caml blocks.

   Code segments are used in signals_asm.c

   TODO: use dichotomic search 
*/

typedef struct segment {
  void *begin;
  void *end;
  struct segment *next;
} segment;

segment *caml_dyn_data_segments = NULL;
segment *caml_dyn_code_segments = NULL;

static segment *segment_cons(void *begin, void *end, segment *tl) {
  segment *lnk = caml_stat_alloc(sizeof(segment));
  lnk->begin = begin;
  lnk->end = end;
  lnk->next = tl;
  return lnk;
}

int caml_is_in_data(void *p) {
  segment *lnk;
  for (lnk = caml_dyn_data_segments; NULL != lnk; lnk = lnk->next)
    if (p >= lnk->begin && p <= lnk->end) return 1;
  return 0;
}
int caml_is_in_code(void *p) {
  segment *lnk;
  for (lnk = caml_dyn_code_segments; NULL != lnk; lnk = lnk->next)
    if (p >= lnk->begin && p <= lnk->end) return 1;
  return 0;
}

extern char caml_globals_map[];

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return (value)caml_globals_map;
}

CAMLprim value caml_natdynlink_globals_inited(value unit)
{
  return Val_int(caml_globals_inited);
}

CAMLprim value caml_natdynlink_open(value filename)
{
  CAMLparam1 (filename);
  CAMLlocal1 (res);
  void *sym;
  void *handle;

  /* TODO: dlclose in case of error... */

  handle = caml_dlopen(String_val(filename), 1);
  
  if (NULL == handle)
    CAMLreturn(caml_copy_string(caml_dlerror()));

  sym = caml_dlsym(handle, "caml_plugin_header");
  if (NULL == sym) 
    CAMLreturn(caml_copy_string("not an OCaml plugin"));

  res = caml_alloc_tuple(2);
  Field(res, 0) = (value) handle;
  Field(res, 1) = (value) (sym);
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_run(void *handle, value symbol) {
  CAMLparam1 (symbol);
  CAMLlocal1 (result);
  void *sym,*sym2;

#define optsym(n) getsym(handle,unit,n,1)
  char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (NULL != sym) caml_register_frametable(sym);
  
  sym = optsym("");
  if (NULL != sym) caml_register_dyn_global(sym);
  
  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (NULL != sym && NULL != sym2)
    caml_dyn_data_segments = segment_cons(sym,sym2,caml_dyn_data_segments); 
  
  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (NULL != sym && NULL != sym2)
    caml_dyn_code_segments = segment_cons(sym,sym2,caml_dyn_code_segments); 
  
  entrypoint = optsym("__entry");
  if (NULL != entrypoint) result = caml_callback((value)(&entrypoint), 0);
  else result = Val_unit;

#undef optsym

  CAMLreturn (result);
}

CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol)
{
  CAMLparam2 (filename, symbol);
  CAMLlocal2 (res, v);
  void *handle;

  /* TODO: dlclose in case of error... */

  handle = caml_dlopen(String_val(filename), 1);
  
  if (NULL == handle) {
    res = caml_alloc(1,1);
    v = caml_copy_string(caml_dlerror());
    Store_field(res, 0, v);
  } else {
    res = caml_alloc(1,0);
    v = caml_natdynlink_run(handle, symbol);
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
