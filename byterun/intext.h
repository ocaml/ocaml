/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Structured input/output */

#ifndef __intext__
#define __intext__

#include "misc.h"
#include "mlvalues.h"
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
#define CODE_STRING8 0x9
#define CODE_STRING32 0xA
#define CODE_DOUBLE_BIG 0xB
#define CODE_DOUBLE_LITTLE 0xC
#define CODE_DOUBLE_ARRAY8_BIG 0xD
#define CODE_DOUBLE_ARRAY8_LITTLE 0xE
#define CODE_DOUBLE_ARRAY32_BIG 0xF
#define CODE_DOUBLE_ARRAY32_LITTLE 0x7

#ifdef BIG_ENDIAN
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

#ifndef INITIAL_EXTERN_TABLE_SIZE
#define INITIAL_EXTERN_TABLE_SIZE 2039
#endif

/* The entry points */

value output_value P((struct channel *, value));
value input_value P((struct channel *));


#endif

