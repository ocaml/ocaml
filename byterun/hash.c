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

/* The generic hashing primitive */

#include "mlvalues.h"
#include "memory.h"
#include "str.h"

static unsigned long hash_accu;
static long hash_univ_limit, hash_univ_count;

static void hash_aux();

value hash_univ_param(count, limit, obj) /* ML */
     value obj, count, limit;
{
  hash_univ_limit = Long_val(limit);
  hash_univ_count = Long_val(count);
  hash_accu = 0;
  hash_aux(obj);
  return Val_long(hash_accu & 0x3FFFFFFF);
  /* The & has two purposes: ensure that the return value is positive
     and give the same result on 32 bit and 64 bit architectures. */
}

#define Alpha 65599
#define Beta 19
#define Combine(new)  (hash_accu = hash_accu * Alpha + (new))
#define Combine_small(new) (hash_accu = hash_accu * Beta + (new))

static void hash_aux(obj)
     value obj;
{
  unsigned char * p;
  mlsize_t i, j;
  tag_t tag;

  hash_univ_limit--;
  if (hash_univ_count < 0 || hash_univ_limit < 0) return;

  if (Is_long(obj)) {
    hash_univ_count--;
    Combine(Long_val(obj));
    return;
  }

  /* Pointers into the heap are well-structured blocks. So are atoms.
     We can inspect the block contents. */
  
  if (Is_atom(obj) || Is_young(obj) || Is_in_heap(obj)) {
    tag = Tag_val(obj);
    switch (tag) {
    case String_tag:
      hash_univ_count--;
      i = string_length(obj);
      for (p = &Byte_u(obj, 0); i > 0; i--, p++)
        Combine_small(*p);
      break;
    case Double_tag:
      /* For doubles, we inspect their binary representation, LSB first.
         The results are consistent among all platforms with IEEE floats. */
      hash_univ_count--;
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, 0), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      break;
    case Double_array_tag:
      hash_univ_count--;
      for (j = 0; j < Bosize_val(obj); j += sizeof(double)) {
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, j + sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, j), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      }
      break;
    case Abstract_tag:
    case Final_tag:
      /* We don't know anything about the contents of the block.
         Better do nothing. */
      break;
    case Infix_tag:
      hash_aux(obj - Infix_offset_val(obj));
      break;
    case Object_tag:
      hash_univ_count--;
      Combine(Oid_val(obj));
      break;
    default:
      hash_univ_count--;
      Combine_small(tag);
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        hash_aux(Field(obj, i));
      }
      break;
    }
    return;
  }

  /* Otherwise, obj is a pointer outside the heap, to an object with
     a priori unknown structure. Use its physical address as hash key. */
  Combine((long) obj);
}
