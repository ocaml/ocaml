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

/* Structured output */

/* The interface of this file is "intext.h" */

#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "reverse.h"

/* To keep track of sharing in externed objects */

typedef unsigned long byteoffset_t;

struct extern_obj {
  byteoffset_t ofs;
  value obj;
};

static byteoffset_t initial_ofs = 1; /* Initial value of object offsets */
static byteoffset_t obj_counter;     /* Number of objects emitted so far */
static struct extern_obj * extern_table = NULL; /* Table of objects seen */
static unsigned long extern_table_size;
static unsigned long extern_table_mask;
static unsigned int extern_hash_shift;
/* extern_table_size, extern_table_mask and extern_hash_shift are such that
      extern_table_size == 1 << (wordsize - extern_hash_shift)
      extern_table_mask == extern_table_size - 1  */

/* Multiplicative Fibonacci hashing (Knuth vol 3, section 6.4, page 518).
   HASH_FACTOR is (sqrt(5) - 1) / 2 * 2^wordsize. */
#ifdef ARCH_SIXTYFOUR
#define HASH_FACTOR 11400714819323198485UL
#else
#define HASH_FACTOR 2654435769UL
#endif
#define Hash(v) (((unsigned long)(v) * HASH_FACTOR) >> extern_hash_shift)

/* Allocate a new extern table */
static void alloc_extern_table(void)
{
  asize_t i;
  extern_table = (struct extern_obj *)
                 caml_stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++) extern_table[i].ofs = 0;
}

/* Grow the extern table */
static void resize_extern_table(void)
{
  asize_t oldsize;
  struct extern_obj * oldtable;
  value obj;
  byteoffset_t ofs;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_hash_shift = extern_hash_shift - 1;
  extern_table_size = 2 * extern_table_size;
  extern_table_mask = extern_table_size - 1;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    ofs = oldtable[i].ofs;
    if (ofs >= initial_ofs) {
      obj = oldtable[i].obj;
      h = Hash(obj);
      while (extern_table[h].ofs > 0) h = (h + 1) & extern_table_mask;
      extern_table[h].ofs = ofs;
      extern_table[h].obj = obj;
    }
  }
  caml_stat_free(oldtable);
}

/* Free the extern table. We keep it around for next call if
   it's still small (we did not grow it) and the initial offset
   does not risk overflowing next time. */
static void free_extern_table(void)
{
  if (extern_table_size > INITIAL_EXTERN_TABLE_SIZE ||
      initial_ofs >= INITIAL_OFFSET_MAX) {
    caml_stat_free(extern_table);
    extern_table = NULL;
  }
}

/* To buffer the output */

static char * extern_block, * extern_ptr, * extern_limit;
static int extern_block_malloced;

static void alloc_extern_block(void)
{
  extern_block = caml_stat_alloc(INITIAL_EXTERN_BLOCK_SIZE);
  extern_limit = extern_block + INITIAL_EXTERN_BLOCK_SIZE;
  extern_ptr = extern_block;
  extern_block_malloced = 1;
}

static void resize_extern_block(int required)
{
  long curr_pos, size, reqd_size;

  if (! extern_block_malloced) {
    initial_ofs += obj_counter;
    free_extern_table();
    caml_failwith("Marshal.to_buffer: buffer overflow");
  }
  curr_pos = extern_ptr - extern_block;
  size = extern_limit - extern_block;
  reqd_size = curr_pos + required;
  while (size <= reqd_size) size *= 2;
  extern_block = caml_stat_resize(extern_block, size);
  extern_limit = extern_block + size;
  extern_ptr = extern_block + curr_pos;
}

/* Write characters, integers, and blocks in the output buffer */

#define Write(c) \
  if (extern_ptr >= extern_limit) resize_extern_block(1); \
  *extern_ptr++ = (c)

static void writeblock(char *data, long int len)
{
  if (extern_ptr + len > extern_limit) resize_extern_block(len);
  memmove(extern_ptr, data, len);
  extern_ptr += len;
}

#if ARCH_FLOAT_ENDIANNESS == 0x01234567 || ARCH_FLOAT_ENDIANNESS == 0x76543210
#define writeblock_float8(data,ndoubles) \
  writeblock((char *)(data), (ndoubles) * 8)
#else
#define writeblock_float8(data,ndoubles) \
  caml_serialize_block_float_8((data), (ndoubles))
#endif

static void writecode8(int code, long int val)
{
  if (extern_ptr + 2 > extern_limit) resize_extern_block(2);
  extern_ptr[0] = code;
  extern_ptr[1] = val;
  extern_ptr += 2;
}

static void writecode16(int code, long int val)
{
  if (extern_ptr + 3 > extern_limit) resize_extern_block(3);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 8;
  extern_ptr[2] = val;
  extern_ptr += 3;
}

static void write32(long int val)
{
  if (extern_ptr + 4 > extern_limit) resize_extern_block(4);
  extern_ptr[0] = val >> 24;
  extern_ptr[1] = val >> 16;
  extern_ptr[2] = val >> 8;
  extern_ptr[3] = val;
  extern_ptr += 4;
}

static void writecode32(int code, long int val)
{
  if (extern_ptr + 5 > extern_limit) resize_extern_block(5);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 24;
  extern_ptr[2] = val >> 16;
  extern_ptr[3] = val >> 8;
  extern_ptr[4] = val;
  extern_ptr += 5;
}

#ifdef ARCH_SIXTYFOUR
static void writecode64(int code, long val)
{
  int i;
  if (extern_ptr + 9 > extern_limit) resize_extern_block(9);
  *extern_ptr ++ = code;
  for (i = 64 - 8; i >= 0; i -= 8) *extern_ptr++ = val >> i;
}
#endif

/* Marshal the given value in the output buffer */

static unsigned long size_32;  /* Size in words of 32-bit block for struct. */
static unsigned long size_64;  /* Size in words of 64-bit block for struct. */

static int extern_ignore_sharing; /* Flag to ignore sharing */
static int extern_closures;     /* Flag to allow externing code pointers */

static void extern_invalid_argument(char *msg)
{
  if (extern_block_malloced) caml_stat_free(extern_block);
  initial_ofs += obj_counter;
  free_extern_table();
  caml_invalid_argument(msg);
}

static void extern_rec(value v)
{
 tailcall:
  if (Is_long(v)) {
    long n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      Write(PREFIX_SMALL_INT + n);
    } else if (n >= -(1 << 7) && n < (1 << 7)) {
      writecode8(CODE_INT8, n);
    } else if (n >= -(1 << 15) && n < (1 << 15)) {
      writecode16(CODE_INT16, n);
#ifdef ARCH_SIXTYFOUR
    } else if (n < -(1L << 31) || n >= (1L << 31)) {
      writecode64(CODE_INT64, n);
#endif
    } else
      writecode32(CODE_INT32, n);
    return;
  }
  if (Is_young(v) || Is_in_heap(v) || Is_atom(v)) {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);
    asize_t h;

    if (tag == Forward_tag) {
      value f = Forward_val (v);
      if (Is_block (f) && (Is_young (f) || Is_in_heap (f))
          && (Tag_val (f) == Forward_tag || Tag_val (f) == Lazy_tag
              || Tag_val (f) == Double_tag)){
        /* Do not short-circuit the pointer. */
      }else{
        v = f;
        goto tailcall;
      }
    }
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      if (tag < 16) {
        Write(PREFIX_SMALL_BLOCK + tag);
      } else {
        writecode32(CODE_BLOCK32, hd);
      }
      return;
    }
    /* Check if already seen */
    if (! extern_ignore_sharing && tag != Infix_tag) {
      if (2 * obj_counter >= extern_table_size) resize_extern_table();
      h = Hash(v);
      while (extern_table[h].ofs >= initial_ofs) {
        if (extern_table[h].obj == v) {
          byteoffset_t d = obj_counter - (extern_table[h].ofs - initial_ofs);
          if (d < 0x100) {
            writecode8(CODE_SHARED8, d);
          } else if (d < 0x10000) {
            writecode16(CODE_SHARED16, d);
          } else {
            writecode32(CODE_SHARED32, d);
          }
          return;
        }
        h = (h + 1) & extern_table_mask;
      }
      /* Not seen yet. Record the object */
      extern_table[h].ofs = initial_ofs + obj_counter;
      extern_table[h].obj = v;
      obj_counter++;
    }
    /* Output the contents of the object */
    switch(tag) {
    case String_tag: {
      mlsize_t len = caml_string_length(v);
      if (len < 0x20) {
        Write(PREFIX_SMALL_STRING + len);
      } else if (len < 0x100) {
        writecode8(CODE_STRING8, len);
      } else {
        writecode32(CODE_STRING32, len);
      }
      writeblock(String_val(v), len);
      size_32 += 1 + (len + 4) / 4;
      size_64 += 1 + (len + 8) / 8;
      break;
    }
    case Double_tag: {
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      Write(CODE_DOUBLE_NATIVE);
      writeblock_float8((double *) v, 1);
      size_32 += 1 + 2;
      size_64 += 1 + 1;
      break;
    }
    case Double_array_tag: {
      mlsize_t nfloats;
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      nfloats = Wosize_val(v) / Double_wosize;
      if (nfloats < 0x100) {
        writecode8(CODE_DOUBLE_ARRAY8_NATIVE, nfloats);
      } else {
        writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
      }
      writeblock_float8((double *) v, nfloats);
      size_32 += 1 + nfloats * 2;
      size_64 += 1 + nfloats;
      break;
    }
    case Abstract_tag:
      extern_invalid_argument("output_value: abstract value (Abstract)");
      break;
    case Infix_tag:
      writecode32(CODE_INFIXPOINTER, Infix_offset_hd(hd));
      extern_rec(v - Infix_offset_hd(hd));
      break;
    /* Use default case for objects
    case Object_tag:
      extern_invalid_argument("output_value: object value");
      break;
    */
    case Custom_tag: {
      unsigned long sz_32, sz_64;
      char * ident = Custom_ops_val(v)->identifier;
      void (*serialize)(value v, unsigned long * wsize_32,
                        unsigned long * wsize_64)
        = Custom_ops_val(v)->serialize;
      if (serialize == NULL) 
        extern_invalid_argument("output_value: abstract value (Custom)");
      Write(CODE_CUSTOM);
      writeblock(ident, strlen(ident) + 1);
      Custom_ops_val(v)->serialize(v, &sz_32, &sz_64);
      size_32 += 2 + ((sz_32 + 3) >> 2);  /* header + ops + data */
      size_64 += 2 + ((sz_64 + 7) >> 3);
      break;
    }
    default: {
      mlsize_t i;
      if (tag < 16 && sz < 8) {
        Write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
#ifdef ARCH_SIXTYFOUR
      } else if (hd >= (1UL << 32)) {
        writecode64(CODE_BLOCK64, Whitehd_hd (hd));
#endif
      } else {
        writecode32(CODE_BLOCK32, Whitehd_hd (hd));
      }
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      for (i = 0; i < sz - 1; i++) extern_rec(Field(v, i));
      v = Field(v, i);
      goto tailcall;
      }
    }
    return;
  }
  if ((char *) v >= caml_code_area_start && (char *) v < caml_code_area_end) {
    if (!extern_closures)
      extern_invalid_argument("output_value: functional value");
    writecode32(CODE_CODEPOINTER, (char *) v - caml_code_area_start);
    writeblock((char *) caml_code_checksum(), 16);
    return;
  }
  extern_invalid_argument("output_value: abstract value (outside heap)");
}

enum { NO_SHARING = 1, CLOSURES = 2 };
static int extern_flags[] = { NO_SHARING, CLOSURES };

static long extern_value(value v, value flags)
{
  long res_len;
  int fl;
  /* Parse flag list */
  fl = caml_convert_flag_list(flags, extern_flags);
  extern_ignore_sharing = fl & NO_SHARING;
  extern_closures = fl & CLOSURES;
  /* Allocate hashtable of objects already seen, if needed */
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  extern_table_mask = extern_table_size - 1;
  extern_hash_shift = 8 * sizeof(value) - INITIAL_EXTERN_TABLE_SIZE_LOG2;
  if (extern_table == NULL) {
    alloc_extern_table();
    initial_ofs = 1;
  }
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  /* Write magic number */
  write32(Intext_magic_number);
  /* Set aside space for the sizes */
  extern_ptr += 4*4;
  /* Marshal the object */
  extern_rec(v);
  /* Update initial offset for next call to extern_value(),
     if we decide to keep the table of shared objects. */
  initial_ofs += obj_counter;
  /* Free the table of shared objects (if needed) */
  free_extern_table();
  /* Write the sizes */
  res_len = extern_ptr - extern_block;
#ifdef ARCH_SIXTYFOUR
  if (res_len >= (1L << 32) ||
      size_32 >= (1L << 32) || size_64 >= (1L << 32)) {
    /* The object is so big its size cannot be written in the header.
       Besides, some of the array lengths or string lengths or shared offsets
       it contains may have overflowed the 32 bits used to write them. */
    caml_failwith("output_value: object too big");
  }
#endif
  extern_ptr = extern_block + 4;
  write32(res_len - 5*4);
  write32(obj_counter);
  write32(size_32);
  write32(size_64);
  /* Result is res_len bytes starting at extern_block */
  return res_len;
}

void caml_output_val(struct channel *chan, value v, value flags)
{
  long len;
  char * block;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("output_value: not a binary channel");
  alloc_extern_block();
  len = extern_value(v, flags);
  /* During [caml_really_putblock], concurrent [caml_output_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [extern_block] may change. So, save the pointer in a local variable. */
  block = extern_block;
  caml_really_putblock(chan, extern_block, len);
  caml_stat_free(block);
}

CAMLprim value caml_output_value(value vchan, value v, value flags)
{
  CAMLparam3 (vchan, v, flags);
  struct channel * channel = Channel(vchan);

  Lock(channel);
  caml_output_val(channel, v, flags);
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_output_value_to_string(value v, value flags)
{
  long len;
  value res;
  alloc_extern_block();
  len = extern_value(v, flags);
  res = caml_alloc_string(len);
  memmove(String_val(res), extern_block, len);
  caml_stat_free(extern_block);
  return res;
}

CAMLprim value caml_output_value_to_buffer(value buf, value ofs, value len,
                                           value v, value flags)
{
  long len_res;
  extern_block = &Byte(buf, Long_val(ofs));
  extern_limit = extern_block + Long_val(len);
  extern_ptr = extern_block;
  extern_block_malloced = 0;
  len_res = extern_value(v, flags);
  return Val_long(len_res);
}

CAMLexport void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ long * len)
{
  long len_res;
  alloc_extern_block();
  len_res = extern_value(v, flags);
  *buf = extern_block;
  *len = len_res;
}

CAMLexport long caml_output_value_to_block(value v, value flags,
                                           char * buf, long len)
{
  long len_res;
  extern_block = buf;
  extern_limit = extern_block + len;
  extern_ptr = extern_block;
  extern_block_malloced = 0;
  len_res = extern_value(v, flags);
  return len_res;
}

/* Functions for writing user-defined marshallers */

CAMLexport void caml_serialize_int_1(int i)
{
  if (extern_ptr + 1 > extern_limit) resize_extern_block(1);
  extern_ptr[0] = i;
  extern_ptr += 1;
}

CAMLexport void caml_serialize_int_2(int i)
{
  if (extern_ptr + 2 > extern_limit) resize_extern_block(2);
  extern_ptr[0] = i >> 8;
  extern_ptr[1] = i;
  extern_ptr += 2;
}

CAMLexport void caml_serialize_int_4(int32 i)
{
  if (extern_ptr + 4 > extern_limit) resize_extern_block(4);
  extern_ptr[0] = i >> 24;
  extern_ptr[1] = i >> 16;
  extern_ptr[2] = i >> 8;
  extern_ptr[3] = i;
  extern_ptr += 4;
}

CAMLexport void caml_serialize_int_8(int64 i)
{
  caml_serialize_block_8(&i, 1);
}

CAMLexport void caml_serialize_float_4(float f)
{
  caml_serialize_block_4(&f, 1);
}

CAMLexport void caml_serialize_float_8(double f)
{
  caml_serialize_block_8(&f, 1);
}

CAMLexport void caml_serialize_block_1(void * data, long len)
{
  if (extern_ptr + len > extern_limit) resize_extern_block(len);
  memmove(extern_ptr, data, len);
  extern_ptr += len;
}

CAMLexport void caml_serialize_block_2(void * data, long len)
{
  if (extern_ptr + 2 * len > extern_limit) resize_extern_block(2 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 2, q += 2)
      Reverse_16(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 2);
  extern_ptr += len * 2;
#endif
}

CAMLexport void caml_serialize_block_4(void * data, long len)
{
  if (extern_ptr + 4 * len > extern_limit) resize_extern_block(4 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 4, q += 4)
      Reverse_32(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 4);
  extern_ptr += len * 4;
#endif
}

CAMLexport void caml_serialize_block_8(void * data, long len)
{
  if (extern_ptr + 8 * len > extern_limit) resize_extern_block(8 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#endif
}

CAMLexport void caml_serialize_block_float_8(void * data, long len)
{
  if (extern_ptr + 8 * len > extern_limit) resize_extern_block(8 * len);
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memmove(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  { 
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  { 
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Permute_64(q, 0x01234567, p, ARCH_FLOAT_ENDIANNESS);
    extern_ptr = q;
  }
#endif
}
