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

/*
   #define --> CAMLextern  (defined with CAMLexport or CAMLprim)
   (rien)  --> CAMLprim
   g       --> global C identifier
   x       --> special case

   SP* signals the special cases:
   - when the identifier was not simply prefixed with [caml_]
   - when the [caml_] version was already used for something else, and
     was renamed out of the way (watch out for [caml_alloc] and
     [caml_array_bound_error] in *.s)
*/

/* a faire:
   - ui_*   (reverifier que win32.c n'en depend pas)
*/


/* **** alloc.c */
#define alloc caml_alloc /*SP*/
#define alloc_small caml_alloc_small
#define alloc_tuple caml_alloc_tuple
#define alloc_string caml_alloc_string
#define alloc_final caml_alloc_final
#define copy_string caml_copy_string
#define alloc_array caml_alloc_array
#define copy_string_array caml_copy_string_array
#define convert_flag_list caml_convert_flag_list

/* **** array.c */

/* **** backtrace.c */
#define backtrace_active caml_backtrace_active
#define backtrace_pos caml_backtrace_pos
#define backtrace_buffer caml_backtrace_buffer
#define backtrace_last_exn caml_backtrace_last_exn
#define print_exception_backtrace caml_print_exception_backtrace

/* **** callback.c */
#define callback_depth caml_callback_depth
#define callbackN_exn caml_callbackN_exn
#define callback_exn caml_callback_exn
#define callback2_exn caml_callback2_exn
#define callback3_exn caml_callback3_exn
#define callback caml_callback
#define callback2 caml_callback2
#define callback3 caml_callback3
#define callbackN caml_callbackN

/* **** compact.c */

/* **** compare.c */
#define compare_unordered caml_compare_unordered

/* **** custom.c */
#define alloc_custom caml_alloc_custom
#define register_custom_operations caml_register_custom_operations

/* **** debugger.c */

/* **** dynlink.c */

/* **** extern.c */
#define output_val caml_output_val
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
#define external_raise caml_external_raise
#define mlraise caml_raise /*SP*/
#define raise_constant caml_raise_constant
#define raise_with_arg caml_raise_with_arg
#define raise_with_string caml_raise_with_string
#define failwith caml_failwith
#define invalid_argument caml_invalid_argument
#define array_bound_error caml_array_bound_error /*SP*/
#define raise_out_of_memory caml_raise_out_of_memory
#define raise_stack_overflow caml_raise_stack_overflow
#define raise_sys_error caml_raise_sys_error
#define raise_end_of_file caml_raise_end_of_file
#define raise_zero_divide caml_raise_zero_divide
#define raise_not_found caml_raise_not_found
#define raise_sys_blocked_io caml_raise_sys_blocked_io
#define init_exceptions caml_init_exceptions
/* **** asmrun/fail.c */
/* **** asmrun/<arch>.s */

/* **** finalise.c */

/* **** fix_code.c */

/* **** floats.c */
/*#define Double_val caml_Double_val             done in mlvalues.h as needed */
/*#define Store_double_val caml_Store_double_val done in mlvalues.h as needed */
#define copy_double caml_copy_double

/* **** freelist.c */

/* **** gc_ctrl.c */

/* **** globroots.c */
#define register_global_root caml_register_global_root
#define remove_global_root caml_remove_global_root

/* **** hash.c */
#define hash_variant caml_hash_variant

/* **** instrtrace.c */

/* **** intern.c */
#define input_val caml_input_val
#define input_val_from_string caml_input_val_from_string
#define input_value_from_malloc caml_input_value_from_malloc
#define input_value_from_block caml_input_value_from_block
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

/* **** ints.c */
#define int32_ops caml_int32_ops
#define copy_int32 caml_copy_int32
/*#define Int64_val caml_Int64_val   *** done in mlvalues.h as needed */
#define int64_ops caml_int64_ops
#define copy_int64 caml_copy_int64
#define nativeint_ops caml_nativeint_ops
#define copy_nativeint caml_copy_nativeint

/* **** io.c */
#define channel_mutex_free caml_channel_mutex_free
#define channel_mutex_lock caml_channel_mutex_lock
#define channel_mutex_unlock caml_channel_mutex_unlock
#define channel_mutex_unlock_exn caml_channel_mutex_unlock_exn
#define all_opened_channels caml_all_opened_channels
#define open_descriptor_in caml_open_descriptor_in /*SP*/
#define open_descriptor_out caml_open_descriptor_out /*SP*/
#define close_channel caml_close_channel /*SP*/
#define channel_size caml_channel_size /*SP*/
#define channel_binary_mode caml_channel_binary_mode
#define flush_partial caml_flush_partial /*SP*/
#define flush caml_flush /*SP*/
#define putword caml_putword
#define putblock caml_putblock
#define really_putblock caml_really_putblock
#define seek_out caml_seek_out /*SP*/
#define pos_out caml_pos_out /*SP*/
#define do_read caml_do_read
#define refill caml_refill
#define getword caml_getword
#define getblock caml_getblock
#define really_getblock caml_really_getblock
#define seek_in caml_seek_in /*SP*/
#define pos_in caml_pos_in /*SP*/
#define input_scan_line caml_input_scan_line /*SP*/
#define finalize_channel caml_finalize_channel
#define alloc_channel caml_alloc_channel
/*#define Val_file_offset caml_Val_file_offset   *** done in io.h as needed */
/*#define File_offset_val caml_File_offset_val   *** done in io.h as needed */

/* **** lexing.c */

/* **** main.c */
/* *** no change */

/* **** major_gc.c */
#define heap_start caml_heap_start
#define heap_end caml_heap_end
#define page_table caml_page_table

/* **** md5.c */
#define md5_string caml_md5_string
#define md5_chan caml_md5_chan
#define MD5Init caml_MD5Init
#define MD5Update caml_MD5Update
#define MD5Final caml_MD5Final
#define MD5Transform caml_MD5Transform

/* **** memory.c */
#define alloc_shr caml_alloc_shr
#define initialize caml_initialize
#define modify caml_modify
#define stat_alloc caml_stat_alloc
#define stat_free caml_stat_free
#define stat_resize caml_stat_resize

/* **** meta.c */

/* **** minor_gc.c */
#define young_start caml_young_start
#define young_end caml_young_end
#define young_ptr caml_young_ptr
#define young_limit caml_young_limit
#define ref_table_ptr caml_ref_table_ptr
#define ref_table_limit caml_ref_table_limit
#define minor_collection caml_minor_collection
#define check_urgent_gc caml_check_urgent_gc

/* **** misc.c */

/* **** obj.c */

/* **** parsing.c */

/* **** prims.c */

/* **** printexc.c */
#define format_caml_exception caml_format_exception /*SP*/

/* **** roots.c */
#define local_roots caml_local_roots
#define scan_roots_hook caml_scan_roots_hook
#define do_local_roots caml_do_local_roots

/* **** signals.c */
#define async_signal_mode caml_async_signal_mode
#define pending_signal caml_pending_signal
#define something_to_do caml_something_to_do
#define enter_blocking_section_hook caml_enter_blocking_section_hook
#define leave_blocking_section_hook caml_leave_blocking_section_hook
#define async_action_hook caml_async_action_hook
#define enter_blocking_section caml_enter_blocking_section
#define leave_blocking_section caml_leave_blocking_section
#define convert_signal_number caml_convert_signal_number
/* **** asmrun/signals.c */
#define garbage_collection caml_garbage_collection

/* **** stacks.c */
#define stack_low caml_stack_low
#define stack_high caml_stack_high
#define stack_threshold caml_stack_threshold
#define extern_sp caml_extern_sp
#define trapsp caml_trapsp
#define trap_barrier caml_trap_barrier

/* **** startup.c */
#define atom_table caml_atom_table
/* **** asmrun/startup.c */
#define static_data_start caml_static_data_start
#define static_data_end caml_static_data_end

/* **** str.c */
#define string_length caml_string_length

/* **** sys.c */
#define sys_error caml_sys_error
#define sys_exit caml_sys_exit

/* **** terminfo.c */

/* **** unix.c  &  win32.c */
#define search_exe_in_path caml_search_exe_in_path

/* **** weak.c */

/* **** asmcomp/asmlink.ml */

/* **** asmcomp/cmmgen.ml */

/* **** asmcomp/asmlink.ml, asmcomp/cmmgen.ml, asmcomp/compilenv.ml */

#endif /* CAML_NAME_SPACE */
#endif /* CAML_COMPATIBILITY_H */
