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

/* Hooks in the OCaml runtime. Warning: these functions/hooks are
   experimental, and might change in the future. */

#ifndef CAML_HOOKS_H
#define CAML_HOOKS_H

#include "misc.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

#ifdef NATIVE_CODE

/* executed just before calling the entry point of a dynamically
   loaded native code module. */
CAMLextern void (*caml_natdynlink_hook)(void* handle, char* unit);

#endif /* NATIVE_CODE */


/* Hooks to profile systhreads. */

/* [caml_st_root_scan_hook(thread_id,bottom,retaddr)] is called before
  scanning the stack of a thread. It is called with [thread_id] equal
  to zero at the end. */

#define CAML_ST_ROOT_SCAN_HOOK(thread_id,bottom,retaddr) \
  if( caml_st_root_scan_hook != NULL ) \
    caml_st_root_scan_hook(thread_id,bottom,retaddr)
CAMLextern void (*caml_st_root_scan_hook)(value thread_id,
                                          char* bottom,
                                          uintnat retaddr);
/*
  [caml_st_change_hook(thread_id, action)] is called everytime an action
  is performed on a thread. The following actions are detected:
 CAML_HOOK_ST_INIT: corresponding thread is the first thread of the
    system. All preceeding OCaml code was executed by it !
 CAML_HOOK_ST_REINIT: corresponding thread is the first thread after a fork.
 CAML_HOOK_ST_YIELD: corresponding thread is blocked until scheduled again.
 CAML_HOOK_ST_SCHEDULE: corresponding thread is being scheduled.
 CAML_HOOK_ST_STOP: corresponding thread is being terminated. It will
    NEVER execute again.
 CAML_HOOK_ST_REGISTER: corresponding thread is declared as a C thread.
    It should be scheduled just after.
 CAML_HOOK_ST_UNREGISTER: corresponding thread is unregistered. It will
    not interact with the OCaml runtime, unless it is registered again, 
    but under a different identifier.
 CAML_HOOK_ST_CREATE: corresponding thread is being created. It is not
    scheduled yet, i.e. another thread is still executing.

Note that we always hold the master lock when [caml_st_change_hook] is
called, but it is unsafe to allocate OCaml data or to assume that the
thread is completely initialized.
 */
#define CAML_HOOK_ST_INIT       0
#define CAML_HOOK_ST_YIELD      1
#define CAML_HOOK_ST_SCHEDULE   2
#define CAML_HOOK_ST_STOP       3
#define CAML_HOOK_ST_REINIT     4
#define CAML_HOOK_ST_REGISTER   5
#define CAML_HOOK_ST_UNREGISTER 6
#define CAML_HOOK_ST_CREATE     7
#define CAML_ST_CHANGE_HOOK(thread_id, change) \
  if( caml_st_change_hook != NULL ) caml_st_change_hook(thread_id, change)
CAMLextern void (*caml_st_change_hook)(value thread_id, int change);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_HOOKS_H */
