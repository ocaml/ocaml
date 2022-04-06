
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

#include <string.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"


CAMLprim value unix_mem_read(value fdv, value memv, value offv, value lenv)
{
    intnat numbytes;
    intnat ret;
    char *data;

    numbytes = Long_val(lenv);
    data = ((char *) (Caml_ba_array_val(memv)->data)) + Long_val(offv);
    caml_enter_blocking_section();
    ret = read(Int_val(fdv), data, (int) numbytes);
    caml_leave_blocking_section();   /* keeps errno intact */
    if (ret == -1) uerror("mem_read", Nothing);
    return Val_long(ret);
}
