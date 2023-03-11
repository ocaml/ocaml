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

#include <caml/mlvalues.h>
#include <caml/debugger.h>
#include <caml/runtime_events.h>
#include "unixsupport.h"
#include <caml/domain.h>
#include <caml/fail.h>

/* Post-fork tasks to be carried out in the parent */
void caml_atfork_parent(pid_t child_pid) {
  CAML_EV_LIFECYCLE(EV_FORK_PARENT, child_pid);
}

/* Post-fork tasks to be carried out in the child */
void caml_atfork_child(void) {
  caml_runtime_events_post_fork();
  CAML_EV_LIFECYCLE(EV_FORK_CHILD, 0);
}

CAMLprim value caml_unix_fork(value unit)
{
  int ret;
  if (caml_domain_is_multicore()) {
    caml_failwith
      ("Unix.fork may not be called while other domains were created");
  }

  ret = fork();

  if (ret == -1) caml_uerror("fork", Nothing);

  if (ret == 0) {
    caml_atfork_child();
    /* the following hook can be redefined in other places */
    caml_atfork_hook();
  } else {
    caml_atfork_parent(ret);
  }

  if (caml_debugger_in_use)
    if ((caml_debugger_fork_mode && ret == 0) ||
        (!caml_debugger_fork_mode && ret != 0))
      caml_debugger_cleanup_fork();

  return Val_int(ret);
}
