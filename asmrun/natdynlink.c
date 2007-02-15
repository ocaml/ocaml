#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

void *getsym(void *handle, char *module, char *name){
  char *fullname = malloc(strlen(module) + strlen(name) + 5);
  sprintf(fullname, "caml%s%s", module, name);
  void *sym = dlsym (handle, fullname);
  if (NULL == sym) {
    printf("natdynlink: cannot find symbol %s\n", fullname);
    exit(2);
  }
  free(fullname);
  return sym;
}

extern char caml_globals_map[];

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return (value)caml_globals_map;
}


CAMLprim value caml_natdynlink_open
(value private, value filename, value symbol)
{
  CAMLparam3 (private, filename, symbol);
  CAMLlocal3 (result, err, tup);

  char *unit = String_val(symbol);

  void *handle =
    dlopen(String_val(filename),
	   (private == Val_true
	    ? RTLD_NOW
	    : RTLD_NOW | RTLD_GLOBAL
	    ));
    
  if (NULL == handle)
    CAMLreturn(caml_copy_string(dlerror()));
  
  caml_register_frametable(getsym(handle,unit,"__frametable"));
  caml_register_dyn_global((value)getsym(handle,unit,""));
  void (*entrypoint)(void) = getsym(handle,unit,"__entry");
  err = caml_callback((value)(&entrypoint), 0);

  CAMLreturn (Val_unit);
}
