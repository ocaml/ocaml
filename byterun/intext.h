/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Structured input/output */

#ifndef CAML_INTEXT_H
#define CAML_INTEXT_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

/* <private> */
#include "io.h"

/* Magic number */

#define Intext_magic_number 0x8495A6BE

/* Codes for the compact format */

#define PREFIX_SMALL_BLOCK 0x80
#define PREFIX_SMALL_INT 0x40
#define PREFIX_SMALL_STRING 0x20
#define CODE_INT8 0x0
#define CODE_INT16 0x1
#define CODE_INT32 0x2
#define CODE_INT64 0x3
#define CODE_SHARED8 0x4
#define CODE_SHARED16 0x5
#define CODE_SHARED32 0x6
#define CODE_BLOCK32 0x8
#define CODE_BLOCK64 0x13
#define CODE_STRING8 0x9
#define CODE_STRING32 0xA
#define CODE_DOUBLE_BIG 0xB
#define CODE_DOUBLE_LITTLE 0xC
#define CODE_DOUBLE_ARRAY8_BIG 0xD
#define CODE_DOUBLE_ARRAY8_LITTLE 0xE
#define CODE_DOUBLE_ARRAY32_BIG 0xF
#define CODE_DOUBLE_ARRAY32_LITTLE 0x7
#define CODE_CODEPOINTER 0x10
#define CODE_INFIXPOINTER 0x11
#define CODE_CUSTOM 0x12

#if ARCH_FLOAT_ENDIANNESS == 0x76543210
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_BIG
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_BIG
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_BIG
#else
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_LITTLE
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_LITTLE
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_LITTLE
#endif

/* Initial sizes of data structures for extern */

#ifndef INITIAL_EXTERN_BLOCK_SIZE
#define INITIAL_EXTERN_BLOCK_SIZE 8192
#endif

#ifndef INITIAL_EXTERN_TABLE_SIZE_LOG2
#define INITIAL_EXTERN_TABLE_SIZE_LOG2 11
#endif

#define INITIAL_EXTERN_TABLE_SIZE (1UL << INITIAL_EXTERN_TABLE_SIZE_LOG2)

/* Maximal value of initial_ofs above which we should start again with
   initial_ofs = 1. Should be low enough to prevent rollover of initial_ofs
   next time we extern a structure. Since a structure contains at most 
   2^N / (2 * sizeof(value)) heap objects (N = 32 or 64 depending on target),
   any value below 2^N - (2^N / (2 * sizeof(value))) suffices.
   We just take 2^(N-1) for simplicity. */

#define INITIAL_OFFSET_MAX (1UL << (8 * sizeof(value) - 1))

/* The entry points */

void caml_output_val (struct channel * chan, value v, value flags);
  /* Output [v] with flags [flags] on the channel [chan]. */

/* </private> */

CAMLextern void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ long * len);
  /* Output [v] with flags [flags] to a memory buffer allocated with
     malloc.  On return, [*buf] points to the buffer and [*len]
     contains the number of bytes in buffer. */
CAMLextern long caml_output_value_to_block(value v, value flags,
                                           char * data, long len);
  /* Output [v] with flags [flags] to a user-provided memory buffer.
     [data] points to the start of this buffer, and [len] is its size
     in bytes.  Return the number of bytes actually written in buffer.
     Raise [Failure] if buffer is too short. */

/* <private> */
value caml_input_val (struct channel * chan);
  /* Read a structured value from the channel [chan]. */
/* </private> */

CAMLextern value caml_input_val_from_string (value str, long ofs);
  /* Read a structured value from the Caml string [str], starting
     at offset [ofs]. */
CAMLextern value caml_input_value_from_malloc(char * data, long ofs);
  /* Read a structured value from a malloced buffer.  [data] points
     to the beginning of the buffer, and [ofs] is the offset of the
     beginning of the externed data in this buffer.  The buffer is
     deallocated with [free] on return, or if an exception is raised. */
CAMLextern value caml_input_value_from_block(char * data, long len);
  /* Read a structured value from a user-provided buffer.  [data] points
     to the beginning of the externed data in this buffer,
     and [len] is the length in bytes of valid data in this buffer.
     The buffer is never deallocated by this routine. */

/* Functions for writing user-defined marshallers */

CAMLextern void caml_serialize_int_1(int i);
CAMLextern void caml_serialize_int_2(int i);
CAMLextern void caml_serialize_int_4(int32 i);
CAMLextern void caml_serialize_int_8(int64 i);
CAMLextern void caml_serialize_float_4(float f);
CAMLextern void caml_serialize_float_8(double f);
CAMLextern void caml_serialize_block_1(void * data, long len);
CAMLextern void caml_serialize_block_2(void * data, long len);
CAMLextern void caml_serialize_block_4(void * data, long len);
CAMLextern void caml_serialize_block_8(void * data, long len);
CAMLextern void caml_serialize_block_float_8(void * data, long len);

CAMLextern int caml_deserialize_uint_1(void);
CAMLextern int caml_deserialize_sint_1(void);
CAMLextern int caml_deserialize_uint_2(void);
CAMLextern int caml_deserialize_sint_2(void);
CAMLextern uint32 caml_deserialize_uint_4(void);
CAMLextern int32 caml_deserialize_sint_4(void);
CAMLextern uint64 caml_deserialize_uint_8(void);
CAMLextern int64 caml_deserialize_sint_8(void);
CAMLextern float caml_deserialize_float_4(void);
CAMLextern double caml_deserialize_float_8(void);
CAMLextern void caml_deserialize_block_1(void * data, long len);
CAMLextern void caml_deserialize_block_2(void * data, long len);
CAMLextern void caml_deserialize_block_4(void * data, long len);
CAMLextern void caml_deserialize_block_8(void * data, long len);
CAMLextern void caml_deserialize_block_float_8(void * data, long len);
CAMLextern void caml_deserialize_error(char * msg);

/* <private> */

/* Auxiliary stuff for sending code pointers */
unsigned char * caml_code_checksum (void);

#ifndef NATIVE_CODE
#include "fix_code.h"
#define caml_code_area_start ((char *) caml_start_code)
#define caml_code_area_end ((char *) caml_start_code + caml_code_size)
#else
extern char * caml_code_area_start, * caml_code_area_end;
#endif

/* </private> */

#endif /* CAML_INTEXT_H */
