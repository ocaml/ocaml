/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _sys_
#define _sys_

#include "misc.h"

#define NO_ARG Val_int(0)

CAMLextern void sys_error (value);
extern void sys_init (char **);
CAMLextern value sys_exit (value);

extern char ** caml_main_argv;

#endif /* _sys_ */
