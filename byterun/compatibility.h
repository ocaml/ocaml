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

/* codage:
   #define --> CAMLextern  (CAMLexport ou CAMLprim)
   (rien)  --> CAMLprim
   g       --> ident global C
*/

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
/*      array_get_addr -> caml_array_get_addr */
/*      array_get_float -> caml_array_get_float */
/*      array_get -> caml_array_get */
/*      array_set_addr -> caml_array_set_addr */
/*      array_set_float -> caml_array_set_float */
/*      array_set -> caml_array_set */
/*      array_unsafe_get_float -> caml_array_unsafe_get_float */
/*      array_unsafe_get -> caml_array_unsafe_get */
/*      array_unsafe_set_addr -> caml_array_unsafe_set_addr */
/*      array_unsafe_set_float -> caml_array_unsafe_set_float */
/*      array_unsafe_set -> caml_array_unsafe_set */
/*      make_vect -> caml_make_vect */
/*      make_array -> caml_make_array */

/* **** backtrace.c */
#define backtrace_active caml_backtrace_active
#define backtrace_pos caml_backtrace_pos
#define backtrace_buffer caml_backtrace_buffer
#define backtrace_last_exn caml_backtrace_last_exn
/*    g init_backtrace -> caml_init_backtrace */
/*    g stash_backtrace -> caml_stash_backtrace */
#define print_exception_backtrace caml_print_exception_backtrace

/* **** callback.c */
#define callback_depth caml_callback_depth /*FIXME CAMLextern sans CAMLexport */
#define callbackN_exn caml_callbackN_exn
#define callback_exn caml_callback_exn
#define callback2_exn caml_callback2_exn
#define callback3_exn caml_callback3_exn
#define callback caml_callback
#define callback2 caml_callback2
#define callback3 caml_callback3
#define callbackN caml_callbackN
/*      register_named_value -> caml_register_named_value */

/* **** compact.c */
/*    g compact_heap -> caml_compact_heap */
/*    g percent_max -> caml_percent_max */
/*    g compact_heap_maybe -> caml_compact_heap_maybe */

/* **** compare.c */
#define compare_unordered caml_compare_unordered
/*      compare -> caml_compare */
/*      equal -> caml_equal */
/*      notequal -> caml_notequal */
/*      lessthan -> caml_lessthan */
/*      lessequal -> caml_lessequal */
/*      greaterthan -> caml_greaterthan */
/*      greaterequal -> caml_greaterequal */

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
/*      lex_engine -> caml_lex_engine */
/*      new_lex_engine -> caml_new_lex_engine */

/* **** main.c */
/* no change */

/* **** major_gc.c */
/*    g percent_free -> caml_percent_free */
/*    g major_heap_increment -> caml_major_heap_increment */
#define heap_start caml_heap_start    /* FIXME CAMLextern sans CAMLexport */
#define heap_end caml_heap_end        /* FIXME CAMLextern sans CAMLexport */
#define page_table caml_page_table    /* FIXME CAMLextern sans CAMLexport */
/*    g page_low -> caml_page_low */
/*    g page_high -> caml_page_high */
/*    g gc_sweep_hp -> caml_gc_sweep_hp */
/*    g gc_phase -> caml_gc_phase */
/*    g allocated_words -> caml_allocated_words */
/*    g extra_heap_memory -> caml_extra_heap_memory */
/*    g fl_size_at_phase_change -> caml_fl_size_at_phase_change */
/*    g darken -> caml_darken */
/*    g major_collection_slice -> caml_major_collection_slice */
/*    g finish_major_cycle -> caml_finish_major_cycle */
/*    g round_heap_chunk_size -> caml_round_heap_chunk_size */
/*    g init_major_heap -> caml_init_major_heap */

/* **** md5.c */
#define md5_string caml_md5_string
#define md5_chan caml_md5_chan
#define MD5Init caml_MD5Init
#define MD5Update caml_MD5Update
#define MD5Final caml_MD5Final
#define MD5Transform caml_MD5Transform

/* **** memory.c */
/*    g alloc_for_heap -> caml_alloc_for_heap */
/*    g free_for_heap -> caml_free_for_heap */
/*    g add_to_heap -> caml_add_to_heap */
/*    g shrink_heap -> caml_shrink_heap */
/*    g allocation_color -> caml_allocation_color */
#define alloc_shr caml_alloc_shr /* FIXME CAMLextern sans CAMLexport */
/*    g adjust_gc_speed -> caml_adjust_gc_speed   FIXME pas CAMLextern ? */
#define initialize caml_initialize
#define modify caml_modify
#define stat_alloc caml_stat_alloc /* FIXME CAMLextern sans CAMLexport */
#define stat_free caml_stat_free /* FIXME CAMLextern sans CAMLexport */
#define stat_resize caml_stat_resize /* FIXME CAMLextern sans CAMLexport */

/* **** meta.c */
/*      get_global_data -> caml_get_global_data */
/*      reify_bytecode -> caml_reify_bytecode  FIXME missing in native code ? */
/*      realloc_global -> caml_realloc_global */
/*    g available_primitives -> caml_available_primitives FIXME useless ? */
/*      get_current_environment -> caml_get_current_environment */
/*      invoke_traced_function -> caml_invoke_traced_function */

/* **** minor_gc.c */
/*    g minor_heap_size -> caml_minor_heap_size */
#define young_start caml_young_start /* FIXME CAMLextern sans CAMLexport */
#define young_end caml_young_end /* FIXME CAMLextern sans CAMLexport */
#define young_ptr caml_young_ptr /* FIXME CAMLextern sans CAMLexport */
#define young_limit caml_young_limit /* FIXME CAMLextern sans CAMLexport */
#define ref_table_ptr caml_ref_table_ptr /* FIXME CAMLextern sans CAMLexport */
#define ref_table_limit caml_ref_table_limit /* FIXME CAMLextern sans CAMLexpo*/
/*    g in_minor_collection -> caml_in_minor_collection */
/*    g set_minor_heap_size -> caml_set_minor_heap_size */
/*    g oldify_one -> caml_oldify_one */
/*    g oldify_mopup -> caml_oldify_mopup */
/*    g empty_minor_heap -> caml_empty_minor_heap */
#define minor_collection caml_minor_collection /*FIXME CAMLextern sans CAMLexp*/
#define check_urgent_gc caml_check_urgent_gc /*FIXME CAMLextern sans CAMLexpor*/
/*    g realloc_ref_table -> caml_realloc_ref_table */

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

/* **** obj.c */
/* **** parsing.c */
/* **** prims.c      (changer Makefile) */
/* **** printexc.c */
/* **** roots.c                     check asmrun */
/* **** signals.c                   check asmrun */

/* **** stacks.c */
#define stack_low caml_stack_low
#define stack_high caml_stack_high
#define stack_threshold caml_stack_threshold
#define extern_sp caml_extern_sp
#define trapsp caml_trapsp
#define trap_barrier caml_trap_barrier
/*    g global_data -> caml_global_data */
/*    g max_stack_size -> caml_max_stack_size */
/*    g init_stack -> caml_init_stack */
/*    g realloc_stack -> caml_realloc_stack */
/*      ensure_stack_capacity -> caml_ensure_stack_capacity */
/*    g change_max_stack_size -> caml_change_max_stack_size */

/* **** startup.c */
#define atom_table caml_atom_table
/*    g attempt_open -> caml_attempt_open */
/*    g read_section_descriptors -> caml_read_section_descriptors */
/*    g seek_optional_section -> caml_seek_optional_section */
/*    g seek_section -> caml_seek_section */

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


/* a supprimer (support Mac OS 9): */
/* **** macintosh.c */
/* **** mpwtool.c */
/* **** rotatecursor.c */


#endif /* CAML_NAME_SPACE */

#endif /* CAML_COMPATIBILITY_H */
