/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Structured input, compact format */

#include <string.h>
#include "alloc.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "reverse.h"

static unsigned char * intern_input, * intern_src;
static int intern_input_malloced;
static header_t * intern_dest;
static asize_t obj_counter;
static value * intern_obj_table;
static unsigned int intern_color;
static header_t intern_header;
static value intern_block;

#define Sign_extend_shift ((sizeof(long) - 1) * 8)
#define Sign_extend(x) (((long)(x) << Sign_extend_shift) >> Sign_extend_shift)

#define read8u() (*intern_src++)
#define read8s() Sign_extend(*intern_src++)
#define read16u() \
  (intern_src += 2, \
   (intern_src[-2] << 8) + intern_src[-1])
#define read16s() \
  (intern_src += 2, \
   (Sign_extend(intern_src[-2]) << 8) + intern_src[-1])
#define read32u() \
  (intern_src += 4, \
   (intern_src[-4] << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])
#define read32s() \
  (intern_src += 4, \
   (Sign_extend(intern_src[-4]) << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])

#ifdef ARCH_SIXTYFOUR
static long read64s(void)
{
  long res;
  int i;
  res = 0;
  for (i = 0; i < 8; i++) res = (res << 8) + intern_src[i];
  intern_src += 8;
  return res;
}
#endif

#define readblock(dest,len) \
  (bcopy(intern_src, dest, len), intern_src += len)

static void intern_cleanup(void)
{
  if (intern_input_malloced) stat_free((char *) intern_input);
  if (intern_obj_table != NULL) stat_free((char *) intern_obj_table);
  Hd_val(intern_block) = intern_header; /* Don't confuse the GC */
}

static void intern_rec(value *dest)
{
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, ofs_ind;
  value v, clos;
  asize_t ofs;
  header_t header;
  char cksum[16];

 tailcall:
  code = read8u();
  if (code >= PREFIX_SMALL_INT) {
    if (code >= PREFIX_SMALL_BLOCK) {
      /* Small block */
      tag = code & 0xF;
      size = (code >> 4) & 0x7;
    read_block:
      if (size == 0) {
        v = Atom(tag);
      } else {
        v = Val_hp(intern_dest);
        *dest = v;
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        dest = (value *) (intern_dest + 1);
        *intern_dest = Make_header(size, tag, intern_color);
        intern_dest += 1 + size;
        for(/*nothing*/; size > 1; size--, dest++)
          intern_rec(dest);
        goto tailcall;
      }
    } else {
      /* Small integer */
      v = Val_int(code & 0x3F);
    }
  } else {
    if (code >= PREFIX_SMALL_STRING) {
      /* Small string */
      len = (code & 0x1F);
    read_string:
      size = (len + sizeof(value)) / sizeof(value);
      v = Val_hp(intern_dest);
      if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
      *intern_dest = Make_header(size, String_tag, intern_color);
      intern_dest += 1 + size;
      Field(v, size - 1) = 0;
      ofs_ind = Bsize_wsize(size) - 1;
      Byte(v, ofs_ind) = ofs_ind - len;
      readblock(String_val(v), len);
    } else {
      switch(code) {
      case CODE_INT8:
        v = Val_long(read8s());
        break;
      case CODE_INT16:
        v = Val_long(read16s());
        break;
      case CODE_INT32:
        v = Val_long(read32s());
        break;
      case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
        v = Val_long(read64s());
        break;
#else
        intern_cleanup();
        failwith("input_value: integer too large");
        break;
#endif
      case CODE_SHARED8:
        ofs = read8u();
      read_shared:
        Assert(ofs > 0 && ofs <= obj_counter && intern_obj_table != NULL); 
        v = intern_obj_table[obj_counter - ofs];
        break;
      case CODE_SHARED16:
        ofs = read16u();
        goto read_shared;
      case CODE_SHARED32:
        ofs = read32u();
        goto read_shared;
      case CODE_BLOCK32:
        header = (header_t) read32u();
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
      case CODE_STRING8:
        len = read8u();
        goto read_string;
      case CODE_STRING32:
        len = read32u();
        goto read_string;
      case CODE_DOUBLE_LITTLE:
      case CODE_DOUBLE_BIG:
        if (sizeof(double) != 8) {
          intern_cleanup();
          invalid_argument("input_value: non-standard floats");
        }
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header(Double_wosize, Double_tag, intern_color);
        intern_dest += 1 + Double_wosize;
        readblock((char *) v, 8);
        if (code != CODE_DOUBLE_NATIVE) Reverse_double(v);
        break;
      case CODE_DOUBLE_ARRAY8_LITTLE:
      case CODE_DOUBLE_ARRAY8_BIG:
        len = read8u();
      read_double_array:
#ifdef NATIVE_CODE
        if (sizeof(double) != 8) {
          intern_cleanup();
          invalid_argument("input_value: non-standard floats");
        }
        size = len * Double_wosize;
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header(size, Double_array_tag, intern_color);
        intern_dest += 1 + size;
        readblock((char *) v, len * 8);
        if (code != CODE_DOUBLE_ARRAY8_NATIVE && 
            code != CODE_DOUBLE_ARRAY32_NATIVE) {
          mlsize_t i;
          for (i = 0; i < len; i++) Reverse_double((value)((double *)v + i));
        }
#else
        intern_cleanup();
        failwith("input_value: cannot read float array");
#endif
        break;
      case CODE_DOUBLE_ARRAY32_LITTLE:
      case CODE_DOUBLE_ARRAY32_BIG:
        len = read32u();
        goto read_double_array;
      case CODE_CODEPOINTER:
        ofs = read32u();
        readblock(cksum, 16);
        if (memcmp(cksum, code_checksum(), 16) != 0) {
          intern_cleanup();
          failwith("input_value: code mismatch");
        }
        v = (value) (code_area_start + ofs);
        break;
      case CODE_INFIXPOINTER:
        ofs = read32u();
        intern_rec(&clos);
        v = clos + ofs;
        break;
      default:
        intern_cleanup();
        failwith("input_value: ill-formed message");
      }
    }
  }
  *dest = v;
}

static void intern_alloc(mlsize_t whsize, mlsize_t num_objects)
{
  mlsize_t wosize;

  if (whsize == 0) {
    intern_obj_table = NULL;
  } else {
    wosize = Wosize_whsize(whsize);
    if (wosize > Max_wosize) failwith("intern: structure too big");
    if (wosize < Max_young_wosize) {
      intern_block = alloc(wosize, String_tag);
    } else {
      intern_block = alloc_shr(wosize, String_tag);
    }
    intern_header = Hd_val(intern_block);
    intern_color = Color_hd(intern_header);
    Assert (intern_color == White || intern_color == Black);
    intern_dest = (header_t *) Hp_val(intern_block);
    obj_counter = 0;
    if (num_objects > 0)
      intern_obj_table = (value *) stat_alloc(num_objects * sizeof(value));
    else
      intern_obj_table = NULL;
  }
}

value input_val(struct channel *chan)
{
  uint32 magic;
  mlsize_t block_len, num_objects, size_32, size_64, whsize;
  value res;

  magic = getword(chan);
  if (magic != Intext_magic_number) failwith("input_value: bad object");
  block_len = getword(chan);
  num_objects = getword(chan);
  size_32 = getword(chan);
  size_64 = getword(chan);
  /* Read block from channel */
  intern_input = (unsigned char *) stat_alloc(block_len);
  intern_input_malloced = 1;
  if (really_getblock(chan, (char *)intern_input, block_len) == 0) {
    stat_free((char *) intern_input);
    failwith("input_value: truncated object");
  }
  intern_src = intern_input;
  /* Allocate result */
#ifdef ARCH_SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  intern_alloc(whsize, num_objects);
  /* Fill it in */
  intern_rec(&res);
  /* Free everything */
  stat_free((char *) intern_input);
  if (intern_obj_table != NULL) stat_free((char *) intern_obj_table);
  return res;
}

value input_value(value vchan)        /* ML */
{
  struct channel * chan = Channel(vchan);
  value res;

  Lock(chan);
  res = input_val(chan);
  Unlock(chan);
  return res;
}

value input_val_from_string(value str, long int ofs)
{
  mlsize_t num_objects, size_32, size_64, whsize;
  value obj;

  intern_src = &Byte_u(str, ofs + 2*4);
  intern_input_malloced = 0;
  num_objects = read32u();
  size_32 = read32u();
  size_64 = read32u();
  /* Allocate result */
#ifdef ARCH_SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  Begin_root(str);
    intern_alloc(whsize, num_objects);
  End_roots();
  intern_src = &Byte_u(str, ofs + 5*4); /* If a GC occurred */
  /* Fill it in */
  intern_rec(&obj);
  /* Free everything */
  if (intern_obj_table != NULL) stat_free((char *) intern_obj_table);
  return obj;
}

value input_value_from_string(value str, value ofs) /* ML */
{
  return input_val_from_string(str, Long_val(ofs));
}

value marshal_data_size(value buff, value ofs) /* ML */
{
  uint32 magic;
  mlsize_t block_len;

  intern_src = &Byte_u(buff, Long_val(ofs));
  intern_input_malloced = 0;
  magic = read32u();
  if (magic != Intext_magic_number) failwith("Marshal.data_size: bad object");
  block_len = read32u();
  return Val_long(block_len);
}

/* Return an MD5 checksum of the code area */

#ifdef NATIVE_CODE

#include "md5.h"

unsigned char * code_checksum()
{
  static unsigned char checksum[16];
  static int checksum_computed = 0;

  if (! checksum_computed) {
    struct MD5Context ctx;
    MD5Init(&ctx);
    MD5Update(&ctx,
              (unsigned char *) code_area_start,
              code_area_end - code_area_start);
    MD5Final(checksum, &ctx);
    checksum_computed = 1;
  }
  return checksum;
}

#else

#include "fix_code.h"

unsigned char * code_checksum(void)
{
  return code_md5;
}

#endif
