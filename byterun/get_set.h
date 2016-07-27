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

#include "caml/mlvalues.h"
#include "caml/alloc.h"

static inline value mem_get16(unsigned char* ptr)
{
  intnat res;
  unsigned char b1, b2;
  b1 = ptr[0];
  b2 = ptr[1];
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 8 | b2;
#else
  res = b2 << 8 | b1;
#endif
  return Val_int(res);
}

static inline value mem_get32(unsigned char* ptr)
{
  intnat res;
  unsigned char b1, b2, b3, b4;
  b1 = ptr[0];
  b2 = ptr[1];
  b3 = ptr[2];
  b4 = ptr[3];
#ifdef ARCH_BIG_ENDIAN
  res = b1 << 24 | b2 << 16 | b3 << 8 | b4;
#else
  res = b4 << 24 | b3 << 16 | b2 << 8 | b1;
#endif
  return caml_copy_int32(res);
}

static inline value mem_get64(unsigned char* ptr)
{
  uint64_t res;
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  b1 = ptr[0];
  b2 = ptr[1];
  b3 = ptr[2];
  b4 = ptr[3];
  b5 = ptr[4];
  b6 = ptr[5];
  b7 = ptr[6];
  b8 = ptr[7];
#ifdef ARCH_BIG_ENDIAN
  res = (uint64_t) b1 << 56 | (uint64_t) b2 << 48
        | (uint64_t) b3 << 40 | (uint64_t) b4 << 32
        | (uint64_t) b5 << 24 | (uint64_t) b6 << 16
        | (uint64_t) b7 << 8 | (uint64_t) b8;
#else
  res = (uint64_t) b8 << 56 | (uint64_t) b7 << 48
        | (uint64_t) b6 << 40 | (uint64_t) b5 << 32
        | (uint64_t) b4 << 24 | (uint64_t) b3 << 16
        | (uint64_t) b2 << 8 | (uint64_t) b1;
#endif
  return caml_copy_int64(res);
}

static inline void mem_set16(unsigned char* ptr, value newval)
{
  unsigned char b1, b2;
  intnat val;
  val = Long_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 8;
  b2 = 0xFF & val;
#else
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ptr[0] = b1;
  ptr[1] = b2;
}

static inline void mem_set32(unsigned char* ptr, value newval)
{
  unsigned char b1, b2, b3, b4;
  intnat val;
  val = Int32_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 24;
  b2 = 0xFF & val >> 16;
  b3 = 0xFF & val >> 8;
  b4 = 0xFF & val;
#else
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ptr[0] = b1;
  ptr[1] = b2;
  ptr[2] = b3;
  ptr[3] = b4;
}

static inline void mem_set64(unsigned char* ptr, value newval)
{
  unsigned char b1, b2, b3, b4, b5, b6, b7, b8;
  int64_t val;
  val = Int64_val(newval);
#ifdef ARCH_BIG_ENDIAN
  b1 = 0xFF & val >> 56;
  b2 = 0xFF & val >> 48;
  b3 = 0xFF & val >> 40;
  b4 = 0xFF & val >> 32;
  b5 = 0xFF & val >> 24;
  b6 = 0xFF & val >> 16;
  b7 = 0xFF & val >> 8;
  b8 = 0xFF & val;
#else
  b8 = 0xFF & val >> 56;
  b7 = 0xFF & val >> 48;
  b6 = 0xFF & val >> 40;
  b5 = 0xFF & val >> 32;
  b4 = 0xFF & val >> 24;
  b3 = 0xFF & val >> 16;
  b2 = 0xFF & val >> 8;
  b1 = 0xFF & val;
#endif
  ptr[0] = b1;
  ptr[1] = b2;
  ptr[2] = b3;
  ptr[3] = b4;
  ptr[4] = b5;
  ptr[5] = b6;
  ptr[6] = b7;
  ptr[7] = b8;
}
