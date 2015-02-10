/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include "unixsupport.h"
#ifdef UTF16
#include "u8tou16.h"
#endif

CAMLprim value unix_mkdir(path, perm)
     value path, perm;
{
  caml_unix_check_path(path, "mkdir");
#ifdef UTF16
	char * temp=String_val(path);
	WCHAR * wtemp;
	if(is_valid_utf8(temp))
		wtemp = utf8_to_utf16(temp);
	else
		wtemp = ansi_to_utf16(temp);
	if (_wmkdir(wtemp) == -1) uerror("mkdir", path);
	free(wtemp);
#else
  if (_mkdir(String_val(path)) == -1) uerror("mkdir", path);
#endif
  return Val_unit;
}
