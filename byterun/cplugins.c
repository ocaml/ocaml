/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                    Fabrice Le Fessant, INRIA de Paris                  */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Load dynamic plugins indicated in the CAML_CPLUGINS environment
   variable. These plugins can be used to set currently existing
   hooks, such as GC hooks and system calls tracing (see misc.h).

   We also define a set of hooks, that can be used by plugins. Hooks
   prototypes are declared in caml/hooks.h.
 */

#define CAML_INTERNALS
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/hooks.h"
#include "caml/osdeps.h"
#include "caml/version.h"

#include <stdio.h>

#ifdef CAML_WITH_CPLUGINS

value (*caml_cplugins_prim)(int,value,value,value) = NULL;

#define DLL_EXECUTABLE 1
#define DLL_NOT_GLOBAL 0

static struct cplugin_context cplugin_context;

void caml_load_plugin(char *plugin)
{
  void* dll_handle = NULL;

  dll_handle = caml_dlopen(plugin, DLL_EXECUTABLE, DLL_NOT_GLOBAL);
  if( dll_handle != NULL ){
   void (* dll_init)(struct cplugin_context*) =
     caml_dlsym(dll_handle, "caml_cplugin_init");
   if( dll_init != NULL ){
     cplugin_context.plugin=plugin;
     dll_init(&cplugin_context);
   } else {
     caml_dlclose(dll_handle);
   }
  } else {
   fprintf(stderr, "Cannot load C plugin %s\nReason: %s\n",
          plugin, caml_dlerror());
  }
}

void caml_cplugins_load(char *env_variable)
{
  char *plugins = getenv(env_variable);
  if(plugins != NULL){
    char* curs = plugins;
    while(*curs != 0){
        if(*curs == ','){
          if(curs > plugins){
            *curs = 0;
            caml_load_plugin(plugins);
          }
          plugins = curs+1;
        }
        curs++;
    }
    if(curs > plugins) caml_load_plugin(plugins);
  }
}

void caml_cplugins_init(char * exe_name, char **argv)
{
  cplugin_context.api_version = CAML_CPLUGIN_CONTEXT_API;
  cplugin_context.prims_bitmap = CAML_CPLUGINS_PRIMS_BITMAP;
  cplugin_context.exe_name = exe_name;
  cplugin_context.argv = argv;
  cplugin_context.ocaml_version = OCAML_VERSION_STRING;
  caml_cplugins_load("CAML_CPLUGINS");
#ifdef NATIVE_CODE
  caml_cplugins_load("CAML_NATIVE_CPLUGINS");
#else
  caml_cplugins_load("CAML_BYTE_CPLUGINS");
#endif
}

#endif /* CAML_WITH_CPLUGINS */

/* Definitions of hooks that can be used by plugins */

/* Begin only called if compiled with WITH_GC_HOOKS */
DECLARE_HOOK1(caml_alloc_small_hook,mlsize_t wosize);
DECLARE_HOOK3(caml_alloc_shr_begin_hook,
              mlsize_t wosize,tag_t tag,uintnat profinfo);
DECLARE_HOOK1(caml_alloc_shr_end_hook, value v);
/* End only called if compiled with WITH_GC_HOOKS */

DECLARE_HOOK1(caml_set_minor_heap_size_begin_hook, intnat bsz);
DECLARE_HOOK1(caml_set_minor_heap_size_end_hook, intnat success);
DECLARE_HOOK1(caml_empty_minor_heap_hook,);

DECLARE_HOOK1(caml_do_compaction_begin_hook, );
DECLARE_HOOK1(caml_do_compaction_middle_hook, );
DECLARE_HOOK1(caml_do_compaction_end_hook, );

DECLARE_HOOK1(caml_intern_rec_end_hook, intnat success);
DECLARE_HOOK1(caml_intern_alloc_begin_hook, intnat wosize);
DECLARE_HOOK1(caml_intern_alloc_end_hook, void* block);

/* Begin only called in native code */
DECLARE_HOOK2(caml_natdynlink_hook,void* handle, char* unit);
DECLARE_HOOK1(caml_garbage_collection_begin_hook, );
DECLARE_HOOK1(caml_garbage_collection_middle_hook, );
DECLARE_HOOK1(caml_garbage_collection_end_hook, );
/* End only called in native code */

/* GC timing hooks. These can be assigned by the user.
   [caml_minor_gc_begin_hook] must not allocate nor change any heap value.
   The others can allocate and even call back to OCaml code.
*/
DECLARE_HOOK1(caml_major_slice_begin_hook,);
DECLARE_HOOK1(caml_major_slice_end_hook,);
DECLARE_HOOK1(caml_minor_gc_begin_hook,);
DECLARE_HOOK1(caml_minor_gc_end_hook,);
DECLARE_HOOK1(caml_finalise_begin_hook,);
DECLARE_HOOK1(caml_finalise_end_hook,);


DECLARE_HOOK1(caml_major_gc_hook,);


CAMLexport uintnat (*caml_alloc_get_profinfo)(uintnat wosize) = NULL;
DECLARE_HOOK1(caml_final_do_call_begin_hook,);
DECLARE_HOOK1(caml_final_do_call_end_hook,);
DECLARE_HOOK1(caml_execute_signal_begin_hook,);
DECLARE_HOOK1(caml_execute_signal_end_hook,);
