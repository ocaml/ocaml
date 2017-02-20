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

#ifndef CAML_HOOKS_H
#define CAML_HOOKS_H

#include "misc.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

/* Some macros to help declaring, exporting and using hooks */
#define EXTERN_HOOK1(hook_name, arg1)      \
  CAMLextern void (*hook_name)(arg1)
#define DECLARE_HOOK1(hook_name, arg1)     \
  CAMLexport void (*hook_name)(arg1) = NULL
#define MAYBE_HOOK1(hook_name, arg1) \
  do { if( (hook_name) != NULL ){ (hook_name)(arg1); } } while(0)

#define EXTERN_HOOK2(hook_name, arg1,arg2)          \
  CAMLextern void (*hook_name)(arg1,arg2)
#define DECLARE_HOOK2(hook_name, arg1,arg2)         \
  CAMLexport void (*hook_name)(arg1,arg2) = NULL
#define MAYBE_HOOK2(hook_name, arg1,arg2)                   \
  do { if( (hook_name) != NULL ){ (hook_name)(arg1,arg2); } } while(0)

#define EXTERN_HOOK3(hook_name, arg1,arg2,arg3)     \
  CAMLextern void (*hook_name)(arg1,arg2,arg3);
#define DECLARE_HOOK3(hook_name, arg1,arg2,arg3)         \
  CAMLexport void (*hook_name)(arg1,arg2,arg3) = NULL;
#define MAYBE_HOOK3(hook_name, arg1,arg2,arg3)                   \
  do { if( (hook_name) != NULL ){ (hook_name)(arg1,arg2,arg3); } } while(0)

/* [caml_set_minor_heap_size_begin_hook] is called in function
[caml_set_minor_heap_size], with the new size of the minor heap before
any change.  [caml_set_minor_heap_size_end_hook] is called when
leaving the function, either with 0 (success) or -1 (exception) */
EXTERN_HOOK1(caml_set_minor_heap_size_begin_hook, intnat bsz);
EXTERN_HOOK1(caml_set_minor_heap_size_end_hook, intnat success);

/* [caml_do_compaction_begin_hook] is called at the beginning of
function [do_compaction], then [caml_do_compaction_middle_hook]
between pass 3 and 4, and [caml_do_compaction_end_hook] at the end. */
EXTERN_HOOK1(caml_do_compaction_begin_hook,);
EXTERN_HOOK1(caml_do_compaction_middle_hook,);
EXTERN_HOOK1(caml_do_compaction_end_hook,);

EXTERN_HOOK1(caml_intern_alloc_begin_hook, intnat wosize);
EXTERN_HOOK1(caml_intern_alloc_end_hook, void* block);
EXTERN_HOOK1(caml_intern_rec_end_hook, intnat success);

EXTERN_HOOK1(caml_major_slice_begin_hook,);
EXTERN_HOOK1(caml_major_slice_end_hook,);
EXTERN_HOOK1(caml_minor_gc_begin_hook,);
EXTERN_HOOK1(caml_minor_gc_end_hook,);
EXTERN_HOOK1(caml_finalise_begin_hook,);
EXTERN_HOOK1(caml_finalise_end_hook,);

/* [caml_major_gc_hook] is called just between the end of the mark
   phase and the beginning of the sweep phase of the major GC */
EXTERN_HOOK1(caml_major_gc_hook,);

/* Begin only called in native code */

/* [caml_garbage_collection_begin_hook] is called in
   [caml_garbage_collection] (asmrun/signals_asm.c) at entry,
   [caml_garbage_collection_begin_hook] before signal handling and
   [caml_garbage_collection_end_hook] before exit. */
EXTERN_HOOK1(caml_garbage_collection_begin_hook,);
EXTERN_HOOK1(caml_garbage_collection_middle_hook,);
EXTERN_HOOK1(caml_garbage_collection_end_hook,);

/* executed just before calling the entry point of a dynamically
   loaded native code module. Declared in asmrun/natdynlink.c. */
EXTERN_HOOK2(caml_natdynlink_hook,void* handle, char* unit);
/* End only called in native code */


/* WITH_GC_HOOKS: these hooks are always defined, but only used when
   the runtime is compiled with WITH_GC_HOOKS (./configure --gc-hooks) 
*/
/* [caml_alloc_small_hook] is called with the size of the block
   to be allocated in Alloc_small(...) */
EXTERN_HOOK1(caml_alloc_small_hook, mlsize_t wosize);
EXTERN_HOOK3(caml_alloc_shr_begin_hook,
              mlsize_t wosize,tag_t tag,uintnat profinfo);
EXTERN_HOOK1(caml_alloc_shr_end_hook, value v);

#ifdef WITH_GC_HOOKS
#define CAML_ALLOC_SMALL_HOOK(wosize) \
  MAYBE_HOOK1(caml_alloc_small_hook,wosize)
#else
#define CAML_ALLOC_SMALL_HOOK(wosize)
#endif

/* WITH_PROFINFO: these hooks are only called if WITH_PROFINFO is
defined. They are currently used by Spacetime.
 */
CAMLextern uintnat (*caml_alloc_get_profinfo)(uintnat wosize);
EXTERN_HOOK1(caml_final_do_call_begin_hook,);
EXTERN_HOOK1(caml_final_do_call_end_hook,);

EXTERN_HOOK1(caml_execute_signal_begin_hook,);
EXTERN_HOOK1(caml_execute_signal_end_hook,);
  
#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_HOOKS_H */
