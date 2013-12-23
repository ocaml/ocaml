/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <sys/types.h>
#include <mlvalues.h>
#include <memory.h>
#include <fail.h>
#include <signals.h>
#include <io.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#ifdef HAS_TRUNCATE

CAMLprim value unix_truncate(value path, value len)
{
  CAMLparam2(path, len);
  char * p;
  int ret;
  p = caml_stat_alloc_string(path);
  caml_enter_blocking_section();
  ret = truncate(p, Long_val(len));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("truncate", path);
  CAMLreturn(Val_unit);
}

CAMLprim value unix_truncate_64(value path, value len)
{
  CAMLparam2(path, len);
  char * p;
  int ret;
  p = caml_stat_alloc_string(path);
  caml_enter_blocking_section();
  ret = truncate(p, File_offset_val(len));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1)
    uerror("truncate", path);
  CAMLreturn(Val_unit);
}

#else

CAMLprim value unix_truncate(value path, value len)
{ invalid_argument("truncate not implemented"); }

CAMLprim value unix_truncate_64(value path, value len)
{ invalid_argument("truncate not implemented"); }

#endif
