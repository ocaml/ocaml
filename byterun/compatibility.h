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
/* **** asmrun/<arch>.s */
/*    g caml_alloc -> caml_allocN */

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
#define alloc_custom caml_alloc_custom
#define register_custom_operations caml_register_custom_operations
/*    g find_custom_operations -> caml_find_custom_operations */
/*    g final_custom_operations -> caml_final_custom_operations */
/*    g init_custom_operations -> caml_init_custom_operations */

/* **** debugger.c */
/*    g debugger_in_use -> caml_debugger_in_use */
/*    g event_count -> caml_event_count */
/*    g debugger_init -> caml_debugger_init */
/*    g debugger -> caml_debugger */

/* **** dynlink.c */
/*    g prim_table -> caml_prim_table */
/*    g prim_name_table -> caml_prim_name_table */
/*    g shared_libs_path -> caml_shared_libs_path */
/*    g build_primitive_table -> caml_build_primitive_table */
/*      dynlink_open_lib -> caml_dynlink_open_lib */
/*      dynlink_close_lib -> caml_dynlink_close_lib */
/*      dynlink_lookup_symbol -> caml_dynlink_lookup_symbol */
/*      dynlink_add_primitive -> caml_dynlink_add_primitive */
/*      dynlink_get_current_libs -> caml_dynlink_get_current_libs */

/* **** extern.c */
/*    g output_val -> caml_output_val */
/*      output_value -> caml_output_value */
/*      output_value_to_string -> caml_output_value_to_string */
/*      output_value_to_buffer -> caml_output_value_to_buffer */
#define output_value_to_malloc caml_output_value_to_malloc
#define output_value_to_block caml_output_value_to_block
#define serialize_int_1 caml_serialize_int_1
#define serialize_int_2 caml_serialize_int_2
#define serialize_int_4 caml_serialize_int_4
#define serialize_int_8 caml_serialize_int_8
#define serialize_float_4 caml_serialize_float_4
#define serialize_float_8 caml_serialize_float_8
#define serialize_block_1 caml_serialize_block_1
#define serialize_block_2 caml_serialize_block_2
#define serialize_block_4 caml_serialize_block_4
#define serialize_block_8 caml_serialize_block_8
#define serialize_block_float_8 caml_serialize_block_float_8

/* **** fail.c */
#define external_raise caml_external_raise /*FIXME CAMLextern sans export */
/*    g exn_bucket -> caml_exn_bucket */
#define mlraise caml_raise
#define raise_constant caml_raise_constant
#define raise_with_arg caml_raise_with_arg
#define raise_with_string caml_raise_with_string
#define failwith caml_failwith
#define invalid_argument caml_invalid_argument
/*#define array_bound_error caml_array_bound_error FIXME */
#define raise_out_of_memory caml_raise_out_of_memory
#define raise_stack_overflow caml_raise_stack_overflow
#define raise_sys_error caml_raise_sys_error
#define raise_end_of_file caml_raise_end_of_file
#define raise_zero_divide caml_raise_zero_divide
#define raise_not_found caml_raise_not_found
#define raise_sys_blocked_io caml_raise_sys_blocked_io
#define init_exceptions caml_init_exceptions
/* **** asmrun/fail.c */
/*    g Out_of_memory -> caml_Out_of_memory FIXME a faire */
/*    g Sys_error -> caml_Sys_error FIXME a faire */
/*    g Failure -> caml_Failure FIXME a faire */
/*    g Invalid_argument -> caml_Invalid_argument FIXME a faire */
/*    g End_of_file -> caml_End_of_file FIXME a faire */
/*    g Division_by_zero -> caml_Division_by_zero FIXME a faire */
/*    g Not_found -> caml_Not_found FIXME a faire */
/*    g Match_failure -> caml_Match_failure FIXME a faire */
/*    g Sys_blocked_io -> caml_Sys_blocked_io FIXME a faire */
/*    g Stack_overflow -> caml_Stack_overflow FIXME a faire */
/*    g bucket_Out_of_memory -> caml_bucket_Out_of_memory FIXME pkoi extern? */
/*    g bucket_Stack_overflow -> caml_bucket_Stack_overflow FIXME idem */
/*    g raise_caml_exception -> caml_raise_exception */
/* **** asmrun/<arch>.s */
/*    g caml_array_bound_error -> caml_ml_array_bound_error */

/* **** finalise.c */
/* **** fix_code.c */
/* **** floats.c */
/* **** freelist.c */
/* **** gc_ctrl.c */
/* **** globroots.c */
/* **** hash.c */
/* **** instrtrace.c */

/* **** intern.c */
/*    g input_val -> caml_input_val */
/*      input_value -> caml_input_value */
#define input_val_from_string caml_input_val_from_string
/*      input_value_from_string -> caml_input_value_from_string */
#define input_value_from_malloc caml_input_value_from_malloc
#define input_value_from_block caml_input_value_from_block
/*      marshal_data_size -> caml_marshal_data_size */
/*    g code_checksum -> caml_code_checksum */
#define deserialize_uint_1 caml_deserialize_uint_1
#define deserialize_sint_1 caml_deserialize_sint_1
#define deserialize_uint_2 caml_deserialize_uint_2
#define deserialize_sint_2 caml_deserialize_sint_2
#define deserialize_uint_4 caml_deserialize_uint_4
#define deserialize_sint_4 caml_deserialize_sint_4
#define deserialize_uint_8 caml_deserialize_uint_8
#define deserialize_sint_8 caml_deserialize_sint_8
#define deserialize_float_4 caml_deserialize_float_4
#define deserialize_float_8 caml_deserialize_float_8
#define deserialize_block_1 caml_deserialize_block_1
#define deserialize_block_2 caml_deserialize_block_2
#define deserialize_block_4 caml_deserialize_block_4
#define deserialize_block_8 caml_deserialize_block_8
#define deserialize_block_float_8 caml_deserialize_block_float_8
#define deserialize_error caml_deserialize_error

/* **** interp.c */
/*    g interprete -> caml_interprete */

/* **** ints.c */
/*      int_compare -> caml_int_compare */
/*      int_of_string -> caml_int_of_string */
/*      format_int -> caml_format_int */
#define int32_ops caml_int32_ops
#define copy_int32 caml_copy_int32
/*      int32_neg -> caml_int32_neg */
/*      int32_add -> caml_int32_add */
/*      int32_sub -> caml_int32_sub */
/*      int32_mul -> caml_int32_mul */
/*      int32_div -> caml_int32_div */
/*      int32_mod -> caml_int32_mod */
/*      int32_and -> caml_int32_and */
/*      int32_or -> caml_int32_or */
/*      int32_xor -> caml_int32_xor */
/*      int32_shift_left -> caml_int32_shift_left */
/*      int32_shift_right -> caml_int32_shift_right */
/*      int32_shift_right_unsigned -> caml_int32_shift_right_unsigned */
/*      int32_of_int -> caml_int32_of_int */
/*      int32_to_int -> caml_int32_to_int */
/*      int32_of_float -> caml_int32_of_float */
/*      int32_to_float -> caml_int32_to_float */
/*      int32_compare -> caml_int32_compare */
/*      int32_format -> caml_int32_format */
/*      int32_of_string -> caml_int32_of_string */
/*      int32_bits_of_float -> caml_int32_bits_of_float */
/*      int32_float_of_bits -> caml_int32_float_of_bits */
/* #define Int64_val caml_Int64_val   *** done in mlvalues.h as needed */
#define int64_ops caml_int64_ops
#define copy_int64 caml_copy_int64
/*      int64_neg -> caml_int64_neg */
/*      int64_add -> caml_int64_add */
/*      int64_sub -> caml_int64_sub */
/*      int64_mul -> caml_int64_mul */
/*      int64_div -> caml_int64_div */
/*      int64_mod -> caml_int64_mod */
/*      int64_and -> caml_int64_and */
/*      int64_or -> caml_int64_or */
/*      int64_xor -> caml_int64_xor */
/*      int64_shift_left -> caml_int64_shift_left */
/*      int64_shift_right -> caml_int64_shift_right */
/*      int64_shift_right_unsigned -> caml_int64_shift_right_unsigned */
/*      int64_of_int -> caml_int64_of_int */
/*      int64_to_int -> caml_int64_to_int */
/*      int64_of_float -> caml_int64_of_float */
/*      int64_to_float -> caml_int64_to_float */
/*      int64_of_int32 -> caml_int64_of_int32 */
/*      int64_to_int32 -> caml_int64_to_int32 */
/*      int64_of_nativeint -> caml_int64_of_nativeint */
/*      int64_to_nativeint -> caml_int64_to_nativeint */
/*      int64_compare -> caml_int64_compare */
/*      int64_format -> caml_int64_format */
/*      int64_of_string -> caml_int64_of_string */
/*      int64_bits_of_float -> caml_int64_bits_of_float */
/*      int64_float_of_bits -> caml_int64_float_of_bits */
#define nativeint_ops caml_nativeint_ops
#define copy_nativeint caml_copy_nativeint
/*      nativeint_neg -> caml_nativeint_neg */
/*      nativeint_add -> caml_nativeint_add */
/*      nativeint_sub -> caml_nativeint_sub */
/*      nativeint_mul -> caml_nativeint_mul */
/*      nativeint_div -> caml_nativeint_div */
/*      nativeint_mod -> caml_nativeint_mod */
/*      nativeint_and -> caml_nativeint_and */
/*      nativeint_or -> caml_nativeint_or */
/*      nativeint_xor -> caml_nativeint_xor */
/*      nativeint_shift_left -> caml_nativeint_shift_left */
/*      nativeint_shift_right -> caml_nativeint_shift_right */
/*      nativeint_shift_right_unsigned -> caml_nativeint_shift_right_unsigned */
/*      nativeint_of_int -> caml_nativeint_of_int */
/*      nativeint_to_int -> caml_nativeint_to_int */
/*      nativeint_of_float -> caml_nativeint_of_float */
/*      nativeint_to_float -> caml_nativeint_to_float */
/*      nativeint_of_int32 -> caml_nativeint_of_int32 */
/*      nativeint_to_int32 -> caml_nativeint_to_int32 */
/*      nativeint_compare -> caml_nativeint_compare */
/*      nativeint_format -> caml_nativeint_format */
/*      nativeint_of_string -> caml_nativeint_of_string */

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
/*      static_alloc -> caml_static_alloc */
/*      static_free -> caml_static_free */
/*      static_resize -> caml_static_resize */
/*      obj_is_block -> caml_obj_is_block */
/*      obj_tag -> caml_obj_tag */
/*      obj_set_tag -> caml_obj_set_tag */
/*      obj_block -> caml_obj_block */
/*      obj_dup -> caml_obj_dup */
/*      obj_truncate -> caml_obj_truncate */
/*      lazy_is_forward  FIXME primitive a supprimer */
/*      lazy_follow_forward -> caml_lazy_follow_forward */
/*      lazy_make_forward -> caml_lazy_make_forward */

/* **** parsing.c */
/*    g parser_trace -> caml_parser_trace */
/*      parse_engine -> caml_parse_engine */

/* **** prims.c */
/*    g buitin_cprim -> caml_builtin_cprim */
/*    g names_of_builtin_cprim -> caml_names_of_builtin_cprim */

/* **** printexc.c */
#define format_caml_exception caml_format_exception /* FIXME double declar. */
/*    g fatal_uncaught_exception -> caml_fatal_uncaught_exception */

/* **** roots.c */
#define local_roots caml_local_roots
#define scan_roots_hook caml_scan_roots_hook /* FIXME CAMLextern sans export */
/*    g oldify_local_roots -> caml_oldify_local_roots */
/*    g darken_all_roots -> caml_darken_all_roots */
/*    g do_roots -> caml_do_roots */
#define do_local_roots caml_do_local_roots /* FIXME CAMLextern sans CAMLexport*/

/* **** signals.c */
#define async_signal_mode caml_async_signal_mode /* FIXME CAMLextern sans expo*/
#define pending_signal caml_pending_signal /* FIXME CAMLextern sans CAMLexport*/
#define something_to_do caml_something_to_do /* FIXME CAMLextern sans export */
/*    g force_major_slice -> caml_force_major_slice */
/*    g signal_handlers -> caml_signal_handlers */
#define enter_blocking_section_hook caml_enter_blocking_section_hook /* FIXME */
#define leave_blocking_section_hook caml_leave_blocking_section_hook /* FIXME */
#define async_action_hook caml_async_action_hook /* FIXME CAMLextern sans expo*/
/*    g process_event -> caml_process_event */
/*    g execute_signal -> caml_execute_signal */
/* FIXME handle_signal devient static !? */
/*    g urge_major_slice -> caml_urge_major_slice */
#define enter_blocking_section caml_enter_blocking_section
#define leave_blocking_section caml_leave_blocking_section
#define convert_signal_number caml_convert_signal_number
/*      install_signal_handler -> caml_install_signal_handler */
/* **** asmrun/signals.c */
/*    g garbage_collection -> caml_garbage_collection */
/*    g init_signals -> caml_init_signals */

/* **** stacks.c */
/* FIXME stack.h: suppression de [void reset_roots (void);] !? */
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
/*      terminfo_setup -> caml_terminfo_setup   FIXME CAMLprim et CAMLexport */
/*      terminfo_backup -> caml_terminfo_backup FIXME CAMLprim et CAMLexport */
/*      terminfo_standout -> caml_terminfo_standout FIXME CAMLprim et CAMLexpo*/
/*      terminfo_resume -> caml_terminfo_resume  FIXME CAMLprim et CAMLexport */

/* **** unix.c  &  win32.c */
/*    g decompose_path -> caml_decompose_path */
/*    g search_in_path -> caml_search_in_path */
#define search_exe_in_path caml_search_exe_in_path
/*    g search_dll_in_path -> caml_search_dll_in_path */
/*    g aligned_mmap -> caml_aligned_mmap */
/*    g aligned_munmap -> caml_aligned_munmap */
/*    g executable_name -> caml_executable_name */
/*    g win32_signal -> caml_win32_signal */
/*    x expand_command_line -> caml_expand_command_line  private CAMLexport */

/* **** weak.c */
/*    g weak_list_head -> caml_weak_list_head */
/*    g weak_none -> caml_weak_none */
/*      weak_create -> caml_weak_create */
/*      weak_set -> caml_weak_set */
/*      weak_get -> caml_weak_get */
/*      weak_get_copy -> caml_weak_get_copy */
/*      weak_check -> caml_weak_check */


/* a supprimer (support Mac OS 9): */
/* **** macintosh.c */
/* **** mpwtool.c */
/* **** rotatecursor.c */

/* a faire: supprimer les Begin_roots/End_roots dans asmrun */


#endif /* CAML_NAME_SPACE */

#endif /* CAML_COMPATIBILITY_H */
