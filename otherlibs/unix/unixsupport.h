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

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#define Nothing ((value) 0)

extern value unix_error_of_code (int errcode);
extern int code_of_unix_error (value error);
Noreturn extern void unix_error (int errcode, char * cmdname, value arg);
Noreturn extern void uerror (char * cmdname, value arg);

#define UNIX_BUFFER_SIZE 65536

#define DIR_Val(v) *((DIR **) &Field(v, 0))
