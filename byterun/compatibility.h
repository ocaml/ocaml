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
/* **** fail.c                check asmrun */
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
/* **** roots.c                     check asmrun */
/* **** rotatecursor.c */
/* **** signals.c                   check asmrun */
/* **** stacks.c */
/* **** startup.c                   check asmrun */
/* **** str.c */
#define string_length caml_string_length
/*      ml_string_length -> caml_ml_string_length */
/*      create_string -> caml_create_string */
/*      string_get -> caml_string_get */
/*      string_set -> caml_string_set */
/*      string_equal -> caml_string_equal */
/*      string_notequal -> caml_string_notequal */
/*      string_compare -> caml_string_compare */
/*      string_lessthan -> caml_string_lessthan */
/*      string_lessequal -> caml_string_lessequal */
/*      string_greaterthan -> caml_string_greaterthan */
/*      string_greaterequal -> caml_string_greaterequal */
/*      blit_string -> caml_blit_string */
/*      fill_string -> caml_fill_string */
/*      is_printable -> caml_is_printable */
/*      bitvect_test -> caml_bitvect_test */

/* **** sys.c */
#define sys_error caml_sys_error
#define sys_exit caml_sys_exit
/*      sys_open -> caml_sys_open */
/*      sys_close -> caml_sys_close */
/*      sys_file_exists -> caml_sys_file_exists */
/*      sys_remove -> caml_sys_remove */
/*      sys_chdir -> caml_sys_chdir */
/*      sys_getcwd -> caml_sys_getcwd */
/*      sys_getenv -> caml_sys_getenv */
/*      sys_get_argv -> caml_sys_get_argv */
/*    g sys_init -> caml_sys_init */
/*      sys_system_command -> caml_sys_system_command */
/*      sys_time -> caml_sys_time */
/*      sys_random_seed -> caml_sys_random_seed */
/*      sys_get_config -> caml_sys_get_config */
/*      sys_read_directory -> caml_sys_read_directory */

/* **** terminfo.c */
/* **** unix.c */
/* **** weak.c */
/* **** win32.c */


#endif /* CAML_NAME_SPACE */

#endif /* CAML_COMPATIBILITY_H */
