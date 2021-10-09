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
#include <caml/eventring.h>
#include "unixsupport.h"
#include <caml/domain.h>
#include <caml/fail.h>

CAMLprim value unix_fork(value unit)
{
  int ret;
  if (caml_domain_is_multicore()) {
    caml_failwith
      ("Unix.fork may not be called while other domains were created");
  }

  CAML_EV_FLUSH();

  ret = fork();
  if (ret == 0) caml_atfork_hook();
  if (ret == -1) uerror("fork", Nothing);

  if (ret == 0) {
    CAML_EVENTRING_DESTROY();
    CAML_EVENTRING_INIT();
    CAML_EV_LIFECYCLE(EV_FORK_CHILD, 0);
  } else {
    CAML_EV_LIFECYCLE(EV_FORK_PARENT, ret);
  }

  if (caml_debugger_in_use)
    if ((caml_debugger_fork_mode && ret == 0) ||
        (!caml_debugger_fork_mode && ret != 0))
      caml_debugger_cleanup_fork();

  return Val_int(ret);
}
