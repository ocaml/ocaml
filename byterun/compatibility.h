/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*          Damien Doligez, projet Moscova, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
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
#define alloc caml_alloc
#define alloc_small caml_alloc_small
#define alloc_tuple caml_alloc_tuple
#define alloc_string caml_alloc_string
#define alloc_final caml_alloc_final
#define copy_string caml_copy_string
#define alloc_array caml_alloc_array
#define copy_string_array caml_copy_string_array
#define convert_flag_list caml_convert_flag_list
/*      alloc_dummy -> caml_alloc_dummy */
/*      update_dummy -> caml_update_dummy */

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
#define channel_mutex_free caml_channel_mutex_free
#define channel_mutex_lock caml_channel_mutex_lock
#define channel_mutex_unlock caml_channel_mutex_unlock
#define channel_mutex_unlock_exn caml_channel_mutex_unlock_exn
#define all_opened_channels caml_all_opened_channels
#define open_descriptor_in caml_open_descriptor_in
#define open_descriptor_out caml_open_descriptor_out
#define close_channel caml_close_channel
#define channel_size caml_channel_size
#define channel_binary_mode caml_channel_binary_mode
#define flush_partial caml_flush_partial
#define flush caml_flush
#define putword caml_putword
#define putblock caml_putblock
#define really_putblock caml_really_putblock
#define seek_out caml_seek_out
#define pos_out caml_pos_out
#define do_read caml_do_read                      /* FIXME not in io.h */
#define refill caml_refill
#define getword caml_getword
#define getblock caml_getblock
#define really_getblock caml_really_getblock
#define seek_in caml_seek_in
#define pos_in caml_pos_in
#define input_scan_line caml_input_scan_line
#define finalize_channel caml_finalize_channel    /* FIXME not in io.h */
#define alloc_channel caml_alloc_channel
/*      caml_open_descriptor_in -> caml_ml_open_descriptor_in */
/*      caml_open_descriptor_out -> caml_ml_open_descriptor_out */
/*      caml_out_channels_list -> caml_ml_out_channels_list */
/*      channel_descriptor -> caml_channel_descriptor */
/*      caml_close_channel -> caml_ml_close_channel */
/*      caml_channel_size -> caml_ml_channel_size */
/*      caml_channel_size_64 -> caml_ml_channel_size_64 */
/*      caml_set_binary_mode -> caml_ml_set_binary_mode */
/*      caml_flush_partial -> caml_ml_flush_partial */
/*      caml_flush -> caml_ml_flush */
/*      caml_output_char -> caml_ml_output_char */
/*      caml_output_int -> caml_ml_output_int */
/*      caml_output_partial -> caml_ml_output_partial */
/*      caml_output -> caml_ml_output */
/*      caml_seek_out -> caml_ml_seek_out */
/*      caml_seek_out_64 -> caml_ml_seek_out_64 */
/*      caml_pos_out -> caml_ml_pos_out */
/*      caml_pos_out_64 -> caml_ml_pos_out_64 */
/*      caml_input_char -> caml_ml_input_char */
/*      caml_input_int -> caml_ml_input_int */
/*      caml_input -> caml_ml_input */
/*      caml_seek_in -> caml_ml_seek_in */
/*      caml_seek_in_64 -> caml_ml_seek_in_64 */
/*      caml_pos_in -> caml_ml_pos_in */
/*      caml_pos_in_64 -> caml_ml_pos_in_64 */
/*      caml_input_scan_line -> caml_ml_input_scan_line */
/* #define Val_file_offset caml_Val_file_offset   *** done in io.h */
/* #define File_offset_val caml_File_offset_val   *** done in io.h */

/* **** lexing.c */
/* **** macintosh.c   (a supprimer) */
/* **** main.c */
/* **** major_gc.c */
/* **** md5.c */
/* **** memory.c */
/* **** meta.c */
/* **** minor_gc.c */

/* **** misc.c */
/*    g verb_gc -> caml_verb_gc */
/*    g gc_message -> caml_gc_message */
/*    g fatal_error -> caml_fatal_error */
/*    g fatal_error_arg -> caml_fatal_error_arg */
/*    g fatal_error_arg2 -> caml_fatal_error_arg2 */
/*    g aligned_malloc -> caml_aligned_malloc */
/*    g ext_table_init -> caml_ext_table_init */
/*    g ext_table_add -> caml_ext_table_add */
/*    g ext_table_free -> caml_ext_table_free */

/* **** mpwtool.c        (a supprimer) */
/* **** obj.c */
/* **** parsing.c */
/* **** prims.c */
/* **** printexc.c */
/* **** roots.c                     check asmrun */
/* **** rotatecursor.c   (a supprimer) */
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
