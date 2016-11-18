/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Structured input/output */

#ifndef CAML_INTEXT_H
#define CAML_INTEXT_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"

#ifdef CAML_INTERNALS
#include "io.h"

/* Magic number */

#define Intext_magic_number_small 0x8495A6BE
#define Intext_magic_number_big 0x8495A6BF

/* Header format for the "small" model: 20 bytes
       0   "small" magic number
       4   length of marshaled data, in bytes
       8   number of shared blocks
      12   size in words when read on a 32-bit platform
      16   size in words when read on a 64-bit platform
   The 4 numbers are 32 bits each, in big endian.

   Header format for the "big" model: 32 bytes
       0   "big" magic number
       4   four reserved bytes, currently set to 0
       8   length of marshaled data, in bytes
      16   number of shared blocks
      24   size in words when read on a 64-bit platform
   The 3 numbers are 64 bits each, in big endian.
*/

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
#define CODE_SHARED64 0x14
#define CODE_BLOCK32 0x8
#define CODE_BLOCK64 0x13
#define CODE_STRING8 0x9
#define CODE_STRING32 0xA
#define CODE_STRING64 0x15
#define CODE_DOUBLE_BIG 0xB
#define CODE_DOUBLE_LITTLE 0xC
#define CODE_DOUBLE_ARRAY8_BIG 0xD
#define CODE_DOUBLE_ARRAY8_LITTLE 0xE
#define CODE_DOUBLE_ARRAY32_BIG 0xF
#define CODE_DOUBLE_ARRAY32_LITTLE 0x7
#define CODE_DOUBLE_ARRAY64_BIG 0x16
#define CODE_DOUBLE_ARRAY64_LITTLE 0x17
#define CODE_CODEPOINTER 0x10
#define CODE_INFIXPOINTER 0x11
#define CODE_CUSTOM 0x12

#if ARCH_FLOAT_ENDIANNESS == 0x76543210
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_BIG
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_BIG
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_BIG
#define CODE_DOUBLE_ARRAY64_NATIVE CODE_DOUBLE_ARRAY64_BIG
#else
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_LITTLE
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_LITTLE
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_LITTLE
#define CODE_DOUBLE_ARRAY64_NATIVE CODE_DOUBLE_ARRAY64_LITTLE
#endif

/* Size-ing data structures for extern.  Chosen so that
   sizeof(struct trail_block) and sizeof(struct output_block)
   are slightly below 8Kb. */

#define ENTRIES_PER_TRAIL_BLOCK  1025
#define SIZE_EXTERN_OUTPUT_BLOCK 8100

/* The entry points */

void caml_output_val (struct channel * chan, value v, value flags);
  /* Output [v] with flags [flags] on the channel [chan]. */

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ intnat * len);
  /* Output [v] with flags [flags] to a memory buffer allocated with
     malloc.  On return, [*buf] points to the buffer and [*len]
     contains the number of bytes in buffer. */
CAMLextern intnat caml_output_value_to_block(value v, value flags,
                                             char * data, intnat len);
  /* Output [v] with flags [flags] to a user-provided memory buffer.
     [data] points to the start of this buffer, and [len] is its size
     in bytes.  Return the number of bytes actually written in buffer.
     Raise [Failure] if buffer is too short. */

#ifdef CAML_INTERNALS
value caml_input_val (struct channel * chan);
  /* Read a structured value from the channel [chan]. */

extern value caml_input_value_to_outside_heap (value channel);
  /* As for [caml_input_value], but the value is unmarshalled into
     malloc blocks that are not added to the heap.  Not for the
     casual user. */

extern int caml_extern_allow_out_of_heap;
  /* Permit the marshaller to traverse structures that look like OCaml
     values but do not live in the OCaml heap. */

extern value caml_output_value(value vchan, value v, value flags);
#endif /* CAML_INTERNALS */

CAMLextern value caml_input_val_from_string (value str, intnat ofs);
  /* Read a structured value from the OCaml string [str], starting
     at offset [ofs]. */
CAMLextern value caml_input_value_from_malloc(char * data, intnat ofs);
  /* Read a structured value from a malloced buffer.  [data] points
     to the beginning of the buffer, and [ofs] is the offset of the
     beginning of the externed data in this buffer.  The buffer is
     deallocated with [free] on return, or if an exception is raised. */
CAMLextern value caml_input_value_from_block(char * data, intnat len);
  /* Read a structured value from a user-provided buffer.  [data] points
     to the beginning of the externed data in this buffer,
     and [len] is the length in bytes of valid data in this buffer.
     The buffer is never deallocated by this routine. */

/* Functions for writing user-defined marshallers */

CAMLextern void caml_serialize_int_1(int i);
CAMLextern void caml_serialize_int_2(int i);
CAMLextern void caml_serialize_int_4(int32_t i);
CAMLextern void caml_serialize_int_8(int64_t i);
CAMLextern void caml_serialize_float_4(float f);
CAMLextern void caml_serialize_float_8(double f);
CAMLextern void caml_serialize_block_1(void * data, intnat len);
CAMLextern void caml_serialize_block_2(void * data, intnat len);
CAMLextern void caml_serialize_block_4(void * data, intnat len);
CAMLextern void caml_serialize_block_8(void * data, intnat len);
CAMLextern void caml_serialize_block_float_8(void * data, intnat len);

CAMLextern int caml_deserialize_uint_1(void);
CAMLextern int caml_deserialize_sint_1(void);
CAMLextern int caml_deserialize_uint_2(void);
CAMLextern int caml_deserialize_sint_2(void);
CAMLextern uint32_t caml_deserialize_uint_4(void);
CAMLextern int32_t caml_deserialize_sint_4(void);
CAMLextern uint64_t caml_deserialize_uint_8(void);
CAMLextern int64_t caml_deserialize_sint_8(void);
CAMLextern float caml_deserialize_float_4(void);
CAMLextern double caml_deserialize_float_8(void);
CAMLextern void caml_deserialize_block_1(void * data, intnat len);
CAMLextern void caml_deserialize_block_2(void * data, intnat len);
CAMLextern void caml_deserialize_block_4(void * data, intnat len);
CAMLextern void caml_deserialize_block_8(void * data, intnat len);
CAMLextern void caml_deserialize_block_float_8(void * data, intnat len);
CAMLextern void caml_deserialize_error(char * msg);

#ifdef CAML_INTERNALS

/* Auxiliary stuff for sending code pointers */

struct code_fragment {
  char * code_start;
  char * code_end;
  unsigned char digest[16];
  char digest_computed;
};

CAMLextern struct code_fragment * caml_extern_find_code(char *addr);

struct ext_table caml_code_fragments_table;

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_INTEXT_H */
