/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*           Damien Doligez, projet Moscova, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2003 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* definitions for compatibility with old identifiers */

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

/* internal global variables renamed between 4.02.1 and 4.03.0 */
#define caml_stat_top_heap_size Bsize_wsize(caml_stat_top_heap_wsz)
#define caml_stat_heap_size Bsize_wsize(caml_stat_heap_wsz)

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
#define alloc CAML_DEPRECATED("alloc", "caml_alloc") caml_alloc /*SP*/
#define alloc_small CAML_DEPRECATED("alloc_small", "caml_alloc_small") caml_alloc_small
#define alloc_tuple CAML_DEPRECATED("alloc_tuple", "caml_alloc_tuple") caml_alloc_tuple
#define alloc_string CAML_DEPRECATED("alloc_string", "caml_alloc_string") caml_alloc_string
#define alloc_final CAML_DEPRECATED("alloc_final", "caml_alloc_final") caml_alloc_final
#define copy_string CAML_DEPRECATED("copy_string", "caml_copy_string") caml_copy_string
#define alloc_array CAML_DEPRECATED("alloc_array", "caml_alloc_array") caml_alloc_array
#define copy_string_array CAML_DEPRECATED("copy_string_array", "caml_copy_string_array") caml_copy_string_array
#define convert_flag_list CAML_DEPRECATED("convert_flag_list", "caml_convert_flag_list") caml_convert_flag_list

/* **** array.c */

/* **** backtrace.c */
#define backtrace_active CAML_DEPRECATED("backtrace_active", "caml_backtrace_active") caml_backtrace_active
#define backtrace_pos CAML_DEPRECATED("backtrace_pos", "caml_backtrace_pos") caml_backtrace_pos
#define backtrace_buffer CAML_DEPRECATED("backtrace_buffer", "caml_backtrace_buffer") caml_backtrace_buffer
#define backtrace_last_exn CAML_DEPRECATED("backtrace_last_exn", "caml_backtrace_last_exn") caml_backtrace_last_exn
#define print_exception_backtrace CAML_DEPRECATED("print_exception_backtrace", "caml_print_exception_backtrace") caml_print_exception_backtrace

/* **** callback.c */
#define callback_depth CAML_DEPRECATED("callback_depth", "caml_callback_depth") caml_callback_depth
#define callbackN_exn CAML_DEPRECATED("callbackN_exn", "caml_callbackN_exn") caml_callbackN_exn
#define callback_exn CAML_DEPRECATED("callback_exn", "caml_callback_exn") caml_callback_exn
#define callback2_exn CAML_DEPRECATED("callback2_exn", "caml_callback2_exn") caml_callback2_exn
#define callback3_exn CAML_DEPRECATED("callback3_exn", "caml_callback3_exn") caml_callback3_exn
#define callback CAML_DEPRECATED("callback", "caml_callback") caml_callback
#define callback2 CAML_DEPRECATED("callback2", "caml_callback2") caml_callback2
#define callback3 CAML_DEPRECATED("callback3", "caml_callback3") caml_callback3
#define callbackN CAML_DEPRECATED("callbackN", "caml_callbackN") caml_callbackN

/* **** compact.c */

/* **** compare.c */
#define compare_unordered CAML_DEPRECATED("compare_unordered", "caml_compare_unordered") caml_compare_unordered

/* **** custom.c */
#define alloc_custom CAML_DEPRECATED("alloc_custom", "caml_alloc_custom") caml_alloc_custom
#define register_custom_operations CAML_DEPRECATED("register_custom_operations", "caml_register_custom_operations") caml_register_custom_operations

/* **** debugger.c */

/* **** dynlink.c */

/* **** extern.c */
#define output_val CAML_DEPRECATED("output_val", "caml_output_val") caml_output_val
#define output_value_to_malloc CAML_DEPRECATED("output_value_to_malloc", "caml_output_value_to_malloc") caml_output_value_to_malloc
#define output_value_to_block CAML_DEPRECATED("output_value_to_block", "caml_output_value_to_block") caml_output_value_to_block
#define serialize_int_1 CAML_DEPRECATED("serialize_int_1", "caml_serialize_int_1") caml_serialize_int_1
#define serialize_int_2 CAML_DEPRECATED("serialize_int_2", "caml_serialize_int_2") caml_serialize_int_2
#define serialize_int_4 CAML_DEPRECATED("serialize_int_4", "caml_serialize_int_4") caml_serialize_int_4
#define serialize_int_8 CAML_DEPRECATED("serialize_int_8", "caml_serialize_int_8") caml_serialize_int_8
#define serialize_float_4 CAML_DEPRECATED("serialize_float_4", "caml_serialize_float_4") caml_serialize_float_4
#define serialize_float_8 CAML_DEPRECATED("serialize_float_8", "caml_serialize_float_8") caml_serialize_float_8
#define serialize_block_1 CAML_DEPRECATED("serialize_block_1", "caml_serialize_block_1") caml_serialize_block_1
#define serialize_block_2 CAML_DEPRECATED("serialize_block_2", "caml_serialize_block_2") caml_serialize_block_2
#define serialize_block_4 CAML_DEPRECATED("serialize_block_4", "caml_serialize_block_4") caml_serialize_block_4
#define serialize_block_8 CAML_DEPRECATED("serialize_block_8", "caml_serialize_block_8") caml_serialize_block_8
#define serialize_block_float_8 CAML_DEPRECATED("serialize_block_float_8", "caml_serialize_block_float_8") caml_serialize_block_float_8

/* **** fail.c */
#define external_raise CAML_DEPRECATED("external_raise", "caml_external_raise") caml_external_raise
#define mlraise CAML_DEPRECATED("mlraise", "caml_raise") caml_raise /*SP*/
#define raise_constant CAML_DEPRECATED("raise_constant", "caml_raise_constant") caml_raise_constant
#define raise_with_arg CAML_DEPRECATED("raise_with_arg", "caml_raise_with_arg") caml_raise_with_arg
#define raise_with_string CAML_DEPRECATED("raise_with_string", "caml_raise_with_string") caml_raise_with_string
#define failwith CAML_DEPRECATED("failwith", "caml_failwith") caml_failwith
#define invalid_argument CAML_DEPRECATED("invalid_argument", "caml_invalid_argument") caml_invalid_argument
#define array_bound_error CAML_DEPRECATED("array_bound_error", "caml_array_bound_error") caml_array_bound_error /*SP*/
#define raise_out_of_memory CAML_DEPRECATED("raise_out_of_memory", "caml_raise_out_of_memory") caml_raise_out_of_memory
#define raise_stack_overflow CAML_DEPRECATED("raise_stack_overflow", "caml_raise_stack_overflow") caml_raise_stack_overflow
#define raise_sys_error CAML_DEPRECATED("raise_sys_error", "caml_raise_sys_error") caml_raise_sys_error
#define raise_end_of_file CAML_DEPRECATED("raise_end_of_file", "caml_raise_end_of_file") caml_raise_end_of_file
#define raise_zero_divide CAML_DEPRECATED("raise_zero_divide", "caml_raise_zero_divide") caml_raise_zero_divide
#define raise_not_found CAML_DEPRECATED("raise_not_found", "caml_raise_not_found") caml_raise_not_found
#define raise_sys_blocked_io CAML_DEPRECATED("raise_sys_blocked_io", "caml_raise_sys_blocked_io") caml_raise_sys_blocked_io
/* **** runtime/fail_nat.c */
/* **** runtime/<arch>.s */

/* **** finalise.c */

/* **** fix_code.c */

/* **** floats.c */
/*#define Double_val caml_Double_val             done in mlvalues.h as needed */
/*#define Store_double_val caml_Store_double_val done in mlvalues.h as needed */
#define copy_double CAML_DEPRECATED("copy_double", "caml_copy_double") caml_copy_double

/* **** freelist.c */

/* **** gc_ctrl.c */

/* **** globroots.c */
#define register_global_root CAML_DEPRECATED("register_global_root", "caml_register_global_root") caml_register_global_root
#define remove_global_root CAML_DEPRECATED("remove_global_root", "caml_remove_global_root") caml_remove_global_root

/* **** hash.c */
#define hash_variant CAML_DEPRECATED("hash_variant", "caml_hash_variant") caml_hash_variant

/* **** instrtrace.c */

/* **** intern.c */
#define input_val CAML_DEPRECATED("input_val", "caml_input_val") caml_input_val
#define input_val_from_string CAML_DEPRECATED("input_val_from_string", "caml_input_val_from_string") caml_input_val_from_string
#define input_value_from_malloc CAML_DEPRECATED("input_value_from_malloc", "caml_input_value_from_malloc") caml_input_value_from_malloc
#define input_value_from_block CAML_DEPRECATED("input_value_from_block", "caml_input_value_from_block") caml_input_value_from_block
#define deserialize_uint_1 CAML_DEPRECATED("deserialize_uint_1", "caml_deserialize_uint_1") caml_deserialize_uint_1
#define deserialize_sint_1 CAML_DEPRECATED("deserialize_sint_1", "caml_deserialize_sint_1") caml_deserialize_sint_1
#define deserialize_uint_2 CAML_DEPRECATED("deserialize_uint_2", "caml_deserialize_uint_2") caml_deserialize_uint_2
#define deserialize_sint_2 CAML_DEPRECATED("deserialize_sint_2", "caml_deserialize_sint_2") caml_deserialize_sint_2
#define deserialize_uint_4 CAML_DEPRECATED("deserialize_uint_4", "caml_deserialize_uint_4") caml_deserialize_uint_4
#define deserialize_sint_4 CAML_DEPRECATED("deserialize_sint_4", "caml_deserialize_sint_4") caml_deserialize_sint_4
#define deserialize_uint_8 CAML_DEPRECATED("deserialize_uint_8", "caml_deserialize_uint_8") caml_deserialize_uint_8
#define deserialize_sint_8 CAML_DEPRECATED("deserialize_sint_8", "caml_deserialize_sint_8") caml_deserialize_sint_8
#define deserialize_float_4 CAML_DEPRECATED("deserialize_float_4", "caml_deserialize_float_4") caml_deserialize_float_4
#define deserialize_float_8 CAML_DEPRECATED("deserialize_float_8", "caml_deserialize_float_8") caml_deserialize_float_8
#define deserialize_block_1 CAML_DEPRECATED("deserialize_block_1", "caml_deserialize_block_1") caml_deserialize_block_1
#define deserialize_block_2 CAML_DEPRECATED("deserialize_block_2", "caml_deserialize_block_2") caml_deserialize_block_2
#define deserialize_block_4 CAML_DEPRECATED("deserialize_block_4", "caml_deserialize_block_4") caml_deserialize_block_4
#define deserialize_block_8 CAML_DEPRECATED("deserialize_block_8", "caml_deserialize_block_8") caml_deserialize_block_8
#define deserialize_block_float_8 CAML_DEPRECATED("deserialize_block_float_8", "caml_deserialize_block_float_8") caml_deserialize_block_float_8
#define deserialize_error CAML_DEPRECATED("deserialize_error", "caml_deserialize_error") caml_deserialize_error

/* **** interp.c */

/* **** ints.c */
#define int32_ops CAML_DEPRECATED("int32_ops", "caml_int32_ops") caml_int32_ops
#define copy_int32 CAML_DEPRECATED("copy_int32", "caml_copy_int32") caml_copy_int32
/*#define Int64_val caml_Int64_val   *** done in mlvalues.h as needed */
#define int64_ops CAML_DEPRECATED("int64_ops", "caml_int64_ops") caml_int64_ops
#define copy_int64 CAML_DEPRECATED("copy_int64", "caml_copy_int64") caml_copy_int64
#define nativeint_ops CAML_DEPRECATED("nativeint_ops", "caml_nativeint_ops") caml_nativeint_ops
#define copy_nativeint CAML_DEPRECATED("copy_nativeint", "caml_copy_nativeint") caml_copy_nativeint

/* **** io.c */
#define channel_mutex_free CAML_DEPRECATED("channel_mutex_free", "caml_channel_mutex_free") caml_channel_mutex_free
#define channel_mutex_lock CAML_DEPRECATED("channel_mutex_lock", "caml_channel_mutex_lock") caml_channel_mutex_lock
#define channel_mutex_unlock CAML_DEPRECATED("channel_mutex_unlock", "caml_channel_mutex_unlock") caml_channel_mutex_unlock
#define channel_mutex_unlock_exn CAML_DEPRECATED("channel_mutex_unlock_exn", "caml_channel_mutex_unlock_exn") caml_channel_mutex_unlock_exn
#define all_opened_channels CAML_DEPRECATED("all_opened_channels", "caml_all_opened_channels") caml_all_opened_channels
#define open_descriptor_in CAML_DEPRECATED("open_descriptor_in", "caml_open_descriptor_in") caml_open_descriptor_in /*SP*/
#define open_descriptor_out CAML_DEPRECATED("open_descriptor_out", "caml_open_descriptor_out") caml_open_descriptor_out /*SP*/
#define close_channel CAML_DEPRECATED("close_channel", "caml_close_channel") caml_close_channel /*SP*/
#define channel_size CAML_DEPRECATED("channel_size", "caml_channel_size") caml_channel_size /*SP*/
#define channel_binary_mode CAML_DEPRECATED("channel_binary_mode", "caml_channel_binary_mode") caml_channel_binary_mode
#define flush_partial CAML_DEPRECATED("flush_partial", "caml_flush_partial") caml_flush_partial /*SP*/
#define flush CAML_DEPRECATED("flush", "caml_flush") caml_flush /*SP*/
#define putword CAML_DEPRECATED("putword", "caml_putword") caml_putword
#define putblock CAML_DEPRECATED("putblock", "caml_putblock") caml_putblock
#define really_putblock CAML_DEPRECATED("really_putblock", "caml_really_putblock") caml_really_putblock
#define seek_out CAML_DEPRECATED("seek_out", "caml_seek_out") caml_seek_out /*SP*/
#define pos_out CAML_DEPRECATED("pos_out", "caml_pos_out") caml_pos_out /*SP*/
#define do_read CAML_DEPRECATED("do_read", "caml_do_read") caml_do_read
#define refill CAML_DEPRECATED("refill", "caml_refill") caml_refill
#define getword CAML_DEPRECATED("getword", "caml_getword") caml_getword
#define getblock CAML_DEPRECATED("getblock", "caml_getblock") caml_getblock
#define really_getblock CAML_DEPRECATED("really_getblock", "caml_really_getblock") caml_really_getblock
#define seek_in CAML_DEPRECATED("seek_in", "caml_seek_in") caml_seek_in /*SP*/
#define pos_in CAML_DEPRECATED("pos_in", "caml_pos_in") caml_pos_in /*SP*/
#define input_scan_line CAML_DEPRECATED("input_scan_line", "caml_input_scan_line") caml_input_scan_line /*SP*/
#define finalize_channel CAML_DEPRECATED("finalize_channel", "caml_finalize_channel") caml_finalize_channel
#define alloc_channel CAML_DEPRECATED("alloc_channel", "caml_alloc_channel") caml_alloc_channel
/*#define Val_file_offset caml_Val_file_offset   *** done in io.h as needed */
/*#define File_offset_val caml_File_offset_val   *** done in io.h as needed */

/* **** lexing.c */

/* **** main.c */
/* *** no change */

/* **** major_gc.c */
#define heap_start CAML_DEPRECATED("heap_start", "caml_heap_start") caml_heap_start
#define page_table CAML_DEPRECATED("page_table", "caml_page_table") caml_page_table

/* **** md5.c */
#define md5_string CAML_DEPRECATED("md5_string", "caml_md5_string") caml_md5_string
#define md5_chan CAML_DEPRECATED("md5_chan", "caml_md5_chan") caml_md5_chan
#define MD5Init CAML_DEPRECATED("MD5Init", "caml_MD5Init") caml_MD5Init
#define MD5Update CAML_DEPRECATED("MD5Update", "caml_MD5Update") caml_MD5Update
#define MD5Final CAML_DEPRECATED("MD5Final", "caml_MD5Final") caml_MD5Final
#define MD5Transform CAML_DEPRECATED("MD5Transform", "caml_MD5Transform") caml_MD5Transform

/* **** memory.c */
#define alloc_shr CAML_DEPRECATED("alloc_shr", "caml_alloc_shr") caml_alloc_shr
#define initialize CAML_DEPRECATED("initialize", "caml_initialize") caml_initialize
#define modify CAML_DEPRECATED("modify", "caml_modify") caml_modify
#define stat_alloc CAML_DEPRECATED("stat_alloc", "caml_stat_alloc") caml_stat_alloc
#define stat_free CAML_DEPRECATED("stat_free", "caml_stat_free") caml_stat_free
#define stat_resize CAML_DEPRECATED("stat_resize", "caml_stat_resize") caml_stat_resize

/* **** meta.c */

/* **** minor_gc.c */
#define young_start CAML_DEPRECATED("young_start", "caml_young_start") caml_young_start
#define young_end CAML_DEPRECATED("young_end", "caml_young_end") caml_young_end
#define young_ptr CAML_DEPRECATED("young_ptr", "caml_young_ptr") caml_young_ptr
#define young_limit CAML_DEPRECATED("young_limit", "caml_young_limit") caml_young_limit
#define ref_table CAML_DEPRECATED("ref_table", "caml_ref_table") caml_ref_table
#define minor_collection CAML_DEPRECATED("minor_collection", "caml_minor_collection") caml_minor_collection
#define check_urgent_gc CAML_DEPRECATED("check_urgent_gc", "caml_check_urgent_gc") caml_check_urgent_gc

/* **** misc.c */

/* **** obj.c */

/* **** parsing.c */

/* **** prims.c */

/* **** printexc.c */
#define format_caml_exception CAML_DEPRECATED("format_caml_exception", "caml_format_exception") caml_format_exception /*SP*/

/* **** roots.c */
#define local_roots CAML_DEPRECATED("local_roots", "caml_local_roots") caml_local_roots
#define scan_roots_hook CAML_DEPRECATED("scan_roots_hook", "caml_scan_roots_hook") caml_scan_roots_hook
#define do_local_roots CAML_DEPRECATED("do_local_roots", "caml_do_local_roots") caml_do_local_roots

/* **** signals.c */
#define pending_signals CAML_DEPRECATED("pending_signals", "caml_pending_signals") caml_pending_signals
#define something_to_do CAML_DEPRECATED("something_to_do", "caml_something_to_do") caml_something_to_do
#define enter_blocking_section_hook CAML_DEPRECATED("enter_blocking_section_hook", "caml_enter_blocking_section_hook") caml_enter_blocking_section_hook
#define leave_blocking_section_hook CAML_DEPRECATED("leave_blocking_section_hook", "caml_leave_blocking_section_hook") caml_leave_blocking_section_hook
#define enter_blocking_section CAML_DEPRECATED("enter_blocking_section", "caml_enter_blocking_section") caml_enter_blocking_section
#define leave_blocking_section CAML_DEPRECATED("leave_blocking_section", "caml_leave_blocking_section") caml_leave_blocking_section
#define convert_signal_number CAML_DEPRECATED("convert_signal_number", "caml_convert_signal_number") caml_convert_signal_number

/* **** runtime/signals.c */
#define garbage_collection CAML_DEPRECATED("garbage_collection", "caml_garbage_collection") caml_garbage_collection

/* **** stacks.c */
#define stack_low CAML_DEPRECATED("stack_low", "caml_stack_low") caml_stack_low
#define stack_high CAML_DEPRECATED("stack_high", "caml_stack_high") caml_stack_high
#define stack_threshold CAML_DEPRECATED("stack_threshold", "caml_stack_threshold") caml_stack_threshold
#define extern_sp CAML_DEPRECATED("extern_sp", "caml_extern_sp") caml_extern_sp
#define trapsp CAML_DEPRECATED("trapsp", "caml_trapsp") caml_trapsp
#define trap_barrier CAML_DEPRECATED("trap_barrier", "caml_trap_barrier") caml_trap_barrier

/* **** startup.c */
#define atom_table CAML_DEPRECATED("atom_table", "caml_atom_table") caml_atom_table
/* **** runtime/startup_nat.c */
#define static_data_start CAML_DEPRECATED("static_data_start", "caml_static_data_start") caml_static_data_start
#define static_data_end CAML_DEPRECATED("static_data_end", "caml_static_data_end") caml_static_data_end

/* **** str.c */
#define string_length CAML_DEPRECATED("string_length", "caml_string_length") caml_string_length

/* **** sys.c */
#define sys_error CAML_DEPRECATED("sys_error", "caml_sys_error") caml_sys_error

/* **** terminfo.c */

/* **** unix.c  &  win32.c */
#define search_exe_in_path CAML_DEPRECATED("search_exe_in_path", "caml_search_exe_in_path") caml_search_exe_in_path

/* **** weak.c */

/* **** asmcomp/asmlink.ml */

/* **** asmcomp/cmmgen.ml */

/* **** asmcomp/asmlink.ml, asmcomp/cmmgen.ml, asmcomp/compilenv.ml */

/* ************************************************************* */

/* **** otherlibs/bigarray */
#define int8 caml_ba_int8
#define uint8 caml_ba_uint8
#define int16 caml_ba_int16
#define uint16 caml_ba_uint16
#define MAX_NUM_DIMS CAML_DEPRECATED("MAX_NUM_DIMS", "CAML_BA_MAX_NUM_DIMS") CAML_BA_MAX_NUM_DIMS
#define caml_bigarray_kind CAML_DEPRECATED("caml_bigarray_kind", "caml_ba_kind") caml_ba_kind
#define BIGARRAY_FLOAT32 CAML_DEPRECATED("BIGARRAY_FLOAT32", "CAML_BA_FLOAT32") CAML_BA_FLOAT32
#define BIGARRAY_FLOAT64 CAML_DEPRECATED("BIGARRAY_FLOAT64", "CAML_BA_FLOAT64") CAML_BA_FLOAT64
#define BIGARRAY_SINT8 CAML_DEPRECATED("BIGARRAY_SINT8", "CAML_BA_SINT8") CAML_BA_SINT8
#define BIGARRAY_UINT8 CAML_DEPRECATED("BIGARRAY_UINT8", "CAML_BA_UINT8") CAML_BA_UINT8
#define BIGARRAY_SINT16 CAML_DEPRECATED("BIGARRAY_SINT16", "CAML_BA_SINT16") CAML_BA_SINT16
#define BIGARRAY_UINT16 CAML_DEPRECATED("BIGARRAY_UINT16", "CAML_BA_UINT16") CAML_BA_UINT16
#define BIGARRAY_INT32 CAML_DEPRECATED("BIGARRAY_INT32", "CAML_BA_INT32") CAML_BA_INT32
#define BIGARRAY_INT64 CAML_DEPRECATED("BIGARRAY_INT64", "CAML_BA_INT64") CAML_BA_INT64
#define BIGARRAY_CAML_INT CAML_DEPRECATED("BIGARRAY_CAML_INT", "CAML_BA_CAML_INT") CAML_BA_CAML_INT
#define BIGARRAY_NATIVE_INT CAML_DEPRECATED("BIGARRAY_NATIVE_INT", "CAML_BA_NATIVE_INT") CAML_BA_NATIVE_INT
#define BIGARRAY_COMPLEX32 CAML_DEPRECATED("BIGARRAY_COMPLEX32", "CAML_BA_COMPLEX32") CAML_BA_COMPLEX32
#define BIGARRAY_COMPLEX64 CAML_DEPRECATED("BIGARRAY_COMPLEX64", "CAML_BA_COMPLEX64") CAML_BA_COMPLEX64
#define BIGARRAY_KIND_MASK CAML_DEPRECATED("BIGARRAY_KIND_MASK", "CAML_BA_KIND_MASK") CAML_BA_KIND_MASK
#define caml_bigarray_layout CAML_DEPRECATED("caml_bigarray_layout", "caml_ba_layout") caml_ba_layout
#define BIGARRAY_C_LAYOUT CAML_DEPRECATED("BIGARRAY_C_LAYOUT", "CAML_BA_C_LAYOUT") CAML_BA_C_LAYOUT
#define BIGARRAY_FORTRAN_LAYOUT CAML_DEPRECATED("BIGARRAY_FORTRAN_LAYOUT", "CAML_BA_FORTRAN_LAYOUT") CAML_BA_FORTRAN_LAYOUT
#define BIGARRAY_LAYOUT_MASK CAML_DEPRECATED("BIGARRAY_LAYOUT_MASK", "CAML_BA_LAYOUT_MASK") CAML_BA_LAYOUT_MASK
#define caml_bigarray_managed CAML_DEPRECATED("caml_bigarray_managed", "caml_ba_managed") caml_ba_managed
#define BIGARRAY_EXTERNAL CAML_DEPRECATED("BIGARRAY_EXTERNAL", "CAML_BA_EXTERNAL") CAML_BA_EXTERNAL
#define BIGARRAY_MANAGED CAML_DEPRECATED("BIGARRAY_MANAGED", "CAML_BA_MANAGED") CAML_BA_MANAGED
#define BIGARRAY_MAPPED_FILE CAML_DEPRECATED("BIGARRAY_MAPPED_FILE", "CAML_BA_MAPPED_FILE") CAML_BA_MAPPED_FILE
#define BIGARRAY_MANAGED_MASK CAML_DEPRECATED("BIGARRAY_MANAGED_MASK", "CAML_BA_MANAGED_MASK") CAML_BA_MANAGED_MASK
#define caml_bigarray_proxy CAML_DEPRECATED("caml_bigarray_proxy", "caml_ba_proxy") caml_ba_proxy
#define caml_bigarray CAML_DEPRECATED("caml_bigarray", "caml_ba_array") caml_ba_array
#define Bigarray_val CAML_DEPRECATED("Bigarray_val", "Caml_ba_array_val") Caml_ba_array_val
#define Data_bigarray_val CAML_DEPRECATED("Data_bigarray_val", "Caml_ba_data_val") Caml_ba_data_val
#define alloc_bigarray CAML_DEPRECATED("alloc_bigarray", "caml_ba_alloc") caml_ba_alloc
#define alloc_bigarray_dims CAML_DEPRECATED("alloc_bigarray_dims", "caml_ba_alloc_dims") caml_ba_alloc_dims
#define bigarray_map_file CAML_DEPRECATED("bigarray_map_file", "caml_ba_map_file") caml_ba_map_file
#define bigarray_unmap_file CAML_DEPRECATED("bigarray_unmap_file", "caml_ba_unmap_file") caml_ba_unmap_file
#define bigarray_element_size CAML_DEPRECATED("bigarray_element_size", "caml_ba_element_size") caml_ba_element_size
#define bigarray_byte_size CAML_DEPRECATED("bigarray_byte_size", "caml_ba_byte_size") caml_ba_byte_size
#define bigarray_deserialize CAML_DEPRECATED("bigarray_deserialize", "caml_ba_deserialize") caml_ba_deserialize
#define MAX_BIGARRAY_MEMORY CAML_DEPRECATED("MAX_BIGARRAY_MEMORY", "CAML_BA_MAX_MEMORY") CAML_BA_MAX_MEMORY
#define bigarray_create CAML_DEPRECATED("bigarray_create", "caml_ba_create") caml_ba_create
#define bigarray_get_N CAML_DEPRECATED("bigarray_get_N", "caml_ba_get_N") caml_ba_get_N
#define bigarray_get_1 CAML_DEPRECATED("bigarray_get_1", "caml_ba_get_1") caml_ba_get_1
#define bigarray_get_2 CAML_DEPRECATED("bigarray_get_2", "caml_ba_get_2") caml_ba_get_2
#define bigarray_get_3 CAML_DEPRECATED("bigarray_get_3", "caml_ba_get_3") caml_ba_get_3
#define bigarray_get_generic CAML_DEPRECATED("bigarray_get_generic", "caml_ba_get_generic") caml_ba_get_generic
#define bigarray_set_1 CAML_DEPRECATED("bigarray_set_1", "caml_ba_set_1") caml_ba_set_1
#define bigarray_set_2 CAML_DEPRECATED("bigarray_set_2", "caml_ba_set_2") caml_ba_set_2
#define bigarray_set_3 CAML_DEPRECATED("bigarray_set_3", "caml_ba_set_3") caml_ba_set_3
#define bigarray_set_N CAML_DEPRECATED("bigarray_set_N", "caml_ba_set_N") caml_ba_set_N
#define bigarray_set_generic CAML_DEPRECATED("bigarray_set_generic", "caml_ba_set_generic") caml_ba_set_generic
#define bigarray_num_dims CAML_DEPRECATED("bigarray_num_dims", "caml_ba_num_dims") caml_ba_num_dims
#define bigarray_dim CAML_DEPRECATED("bigarray_dim", "caml_ba_dim") caml_ba_dim
#define bigarray_kind CAML_DEPRECATED("bigarray_kind", "caml_ba_kind") caml_ba_kind
#define bigarray_layout CAML_DEPRECATED("bigarray_layout", "caml_ba_layout") caml_ba_layout
#define bigarray_slice CAML_DEPRECATED("bigarray_slice", "caml_ba_slice") caml_ba_slice
#define bigarray_sub CAML_DEPRECATED("bigarray_sub", "caml_ba_sub") caml_ba_sub
#define bigarray_blit CAML_DEPRECATED("bigarray_blit", "caml_ba_blit") caml_ba_blit
#define bigarray_fill CAML_DEPRECATED("bigarray_fill", "caml_ba_fill") caml_ba_fill
#define bigarray_reshape CAML_DEPRECATED("bigarray_reshape", "caml_ba_reshape") caml_ba_reshape
#define bigarray_init CAML_DEPRECATED("bigarray_init", "caml_ba_init") caml_ba_init

#endif /* CAML_NAME_SPACE */
#endif /* CAML_COMPATIBILITY_H */
