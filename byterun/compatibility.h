/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* definitions for compatibility with old identifiers */

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

#ifndef CAML_NAME_SPACE

/* **** alloc.c */
/* **** array.c */
/* **** backtrace.c */
/* **** callback.c */
/* **** compact.c */
/* **** compare.c */
/* **** custom.c */
/* **** debugger.c */
/* **** dynlink.c */
/* **** extern.c */
/* **** fail.c */
/* **** finalise.c */
/* **** fix_code.c */
/* **** floats.c */
/* **** freelist.c */
/* **** gc_ctrl.c */
/* **** globroots.c */
/* **** hash.c */
/* **** instrtrace.c */
/* **** intern.c */
/* **** interp.c */
/* **** ints.c */
/* **** io.c */
/* **** lexing.c */
/* **** macintosh.c */
/* **** main.c */
/* **** major_gc.c */
/* **** md5.c */
/* **** memory.c */
/* **** meta.c */
/* **** minor_gc.c */
/* **** misc.c */
/* **** mpwtool.c */
/* **** obj.c */
/* **** parsing.c */
/* **** prims.c */
/* **** printexc.c */
/* **** roots.c */
/* **** rotatecursor.c */
/* **** signals.c */
/* **** stacks.c */
/* **** startup.c */
/* **** str.c */

/* **** sys.c */
/*      sys_open -> caml_sys_open */
#define sys_error caml_sys_error
#define sys_exit caml_sys_exit

/* **** terminfo.c */
/* **** unix.c */
/* **** weak.c */
/* **** win32.c */


#endif /* CAML_NAME_SPACE */

#endif /* CAML_COMPATIBILITY_H */
