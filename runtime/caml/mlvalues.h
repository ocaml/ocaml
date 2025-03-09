/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MLVALUES_H
#define CAML_MLVALUES_H

#include "config.h"
#include "misc.h"
#include "tsan.h"
#include "camlatomic.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Definitions

  word: Four bytes on 32 and 16 bit architectures,
        eight bytes on 64 bit architectures.
  long: A C integer having the same number of bytes as a word.
  val: The ML representation of something.  A long or a block or a pointer
       outside the heap.  If it is a block, it is the (encoded) address
       of an object.  If it is a long, it is encoded as well.
  block: Something allocated.  It always has a header and some
          fields or some number of bytes (a multiple of the word size).
  field: A word-sized val which is part of a block.
  bp: Pointer to the first byte of a block.  (a char *)
  op: Pointer to the first field of a block.  (a value *)
  hp: Pointer to the header of a block.  (a char *)
  int32_t: Four bytes on all architectures.
  int64_t: Eight bytes on all architectures.

  Remark: A block size is always a multiple of the word size, and at least
          one word plus the header.

  bosize: Size (in bytes) of the "bytes" part.
  wosize: Size (in words) of the "fields" part.
  bhsize: Size (in bytes) of the block with its header.
  whsize: Size (in words) of the block with its header.

  hd: A header.
  tag: The value of the tag field of the header.
  color: The value of the color field of the header.
         This is for use only by the GC.
*/

typedef intnat value;
typedef uintnat header_t;
typedef header_t reserved_t;
typedef uintnat mlsize_t;
typedef unsigned int tag_t;             /* Actually, an unsigned char */
typedef uintnat color_t;
typedef uintnat mark_t;
typedef atomic_intnat atomic_value;
typedef volatile value * value_ptr;
typedef int32_t opcode_t;
typedef opcode_t * code_t;

#include "domain_state.h"

/* Longs vs blocks. */
#define Is_long(x)   (((x) & 1) != 0)
#define Is_block(x)  (((x) & 1) == 0)

/* Conversion macro names are always of the form  "to_from". */
/* Example: Val_long as in "Val from long" or "Val of long". */
#define Val_long(x)     ((intnat) (((uintnat)(x) << 1)) + 1)
#define Long_val(x)     ((x) >> 1)
#define Max_long (((intnat)1 << (8 * sizeof(value) - 2)) - 1)
#define Min_long (-((intnat)1 << (8 * sizeof(value) - 2)))
#define Val_int(x) Val_long(x)
#define Int_val(x) ((int) Long_val(x))
#define Unsigned_long_val(x) ((uintnat)(x) >> 1)
#define Unsigned_int_val(x)  ((int) Unsigned_long_val(x))

/* A 'result' type for OCaml computations. */

/* The [caml_result] type represents the result of computing an OCaml
   term -- either a value or an exception.

   This plays a similar role to the [('a, exn) result] type in OCaml,
   with a different representation. Returning this type, instead of
   raising exceptions directly, lets the caller implement proper
   cleanup and propagate the exception themselves.
*/
typedef struct caml_result_private caml_result;

/* This structure should be considered internal, its definition may
   change in the future. Its public interface is formed of
   - Result_value, Result_exception
   - caml_result_is_exception
   - caml_get_value_or_raise (in fail.h)
*/
struct caml_result_private {
  int is_exception;
  value data;
};

#define Result_value(v) \
  (struct caml_result_private){ .is_exception = 0, .data = v }
#define Result_exception(exn) \
  (struct caml_result_private){ .is_exception = 1, .data = exn }

Caml_inline int caml_result_is_exception(struct caml_result_private result)
{
  return result.is_exception;
}

#define Result_unit Result_value(Val_unit)


/* Structure of the header:

For 16-bit and 32-bit architectures:
     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  31    10 9     8 7   0

For 64-bit architectures:

     +----------+--------+-------+-----+
     | reserved | wosize | color | tag |
     +----------+--------+-------+-----+
bits  63    64-R 63-R  10 9     8 7   0

where 0 <= R <= 31 is HEADER_RESERVED_BITS, set with the
--enable-reserved-header-bits=R argument to configure.

*/

#define HEADER_BITS (sizeof(header_t) * CHAR_BIT)

#define HEADER_TAG_BITS 8
#define HEADER_TAG_MASK ((1ull << HEADER_TAG_BITS) - 1ull)

#define HEADER_COLOR_BITS 2
#define HEADER_COLOR_SHIFT HEADER_TAG_BITS
#define HEADER_COLOR_MASK (((1ull << HEADER_COLOR_BITS) - 1ull) \
                            << HEADER_COLOR_SHIFT)

#define HEADER_WOSIZE_BITS (HEADER_BITS - HEADER_TAG_BITS \
                            - HEADER_COLOR_BITS - HEADER_RESERVED_BITS)
#define HEADER_WOSIZE_SHIFT (HEADER_COLOR_SHIFT  + HEADER_COLOR_BITS)
#define HEADER_WOSIZE_MASK (((1ull << HEADER_WOSIZE_BITS) - 1ull) \
                             << HEADER_WOSIZE_SHIFT)

#define Tag_hd(hd) ((tag_t) ((hd) & HEADER_TAG_MASK))
#define Hd_with_tag(hd, tag) (((hd) &~ HEADER_TAG_MASK) | (tag))
#define Wosize_hd(hd) ((mlsize_t) (((hd) & HEADER_WOSIZE_MASK) \
                                     >> HEADER_WOSIZE_SHIFT))

/* A "clean" header, without reserved or color bits. */
#define Cleanhd_hd(hd) (((header_t)(hd)) & \
                        (HEADER_TAG_MASK | HEADER_WOSIZE_MASK))

#if HEADER_RESERVED_BITS > 0

#define HEADER_RESERVED_SHIFT (HEADER_BITS - HEADER_RESERVED_BITS)
#define Reserved_hd(hd)   (((header_t) (hd)) >> HEADER_RESERVED_SHIFT)
#define Hd_reserved(res)  ((header_t)(res) << HEADER_RESERVED_SHIFT)

#else /* HEADER_RESERVED_BITS is 0 */

#define Reserved_hd(hd)   ((reserved_t)0)
#define Hd_reserved(res)  ((header_t)0)

#endif

/* Color values are pre-shifted */

#define Color_hd(hd) ((hd) & HEADER_COLOR_MASK)
#define Hd_with_color(hd, color) (((hd) &~ HEADER_COLOR_MASK) | (color))

#define Hp_atomic_val(val) ((atomic_uintnat *)(val) - 1)
CAMLno_tsan_for_perf Caml_inline header_t Hd_val(value val)
{
  return atomic_load_explicit(Hp_atomic_val(val), memory_order_relaxed);
}

#define Color_val(val) (Color_hd (Hd_val (val)))

#define Hd_hp(hp) (* ((volatile header_t *) (hp)))      /* Also an l-value. */
#define Hp_val(val) (((volatile header_t *) (val)) - 1)
#define Hp_op(op) (Hp_val (op))
#define Hp_bp(bp) (Hp_val (bp))
#define Val_op(op) ((value) (op))
#define Val_hp(hp) ((value) (((header_t *) (hp)) + 1))
#define Op_hp(hp) ((volatile value *) Val_hp (hp))
#define Bp_hp(hp) ((char *) Val_hp (hp))

#define Num_tags (1ull << HEADER_TAG_BITS)
#define Max_wosize ((1ull << HEADER_WOSIZE_BITS) - 1ull)

#define Wosize_val(val) (Wosize_hd (Hd_val (val)))
#define Wosize_op(op) (Wosize_val (op))
#define Wosize_bp(bp) (Wosize_val (bp))
#define Wosize_hp(hp) (Wosize_hd (Hd_hp (hp)))
#define Whsize_wosize(sz) ((sz) + 1)
#define Wosize_whsize(sz) ((sz) - 1)
#define Wosize_bhsize(sz) ((sz) / sizeof (value) - 1)
#define Bsize_wsize(sz) ((sz) * sizeof (value))
#define Wsize_bsize(sz) ((sz) / sizeof (value))
#define Bhsize_wosize(sz) (Bsize_wsize (Whsize_wosize (sz)))
#define Bhsize_bosize(sz) ((sz) + sizeof (header_t))
#define Bosize_val(val) (Bsize_wsize (Wosize_val (val)))
#define Bosize_op(op) (Bosize_val (Val_op (op)))
#define Bosize_bp(bp) (Bosize_val (Val_bp (bp)))
#define Bosize_hd(hd) (Bsize_wsize (Wosize_hd (hd)))
#define Whsize_hp(hp) (Whsize_wosize (Wosize_hp (hp)))
#define Whsize_val(val) (Whsize_hp (Hp_val (val)))
#define Whsize_bp(bp) (Whsize_val (Val_bp (bp)))
#define Whsize_hd(hd) (Whsize_wosize (Wosize_hd (hd)))
#define Bhsize_hp(hp) (Bsize_wsize (Whsize_hp (hp)))
#define Bhsize_hd(hd) (Bsize_wsize (Whsize_hd (hd)))

#define Reserved_val(val) (Reserved_hd (Hd_val (val)))

#ifdef ARCH_BIG_ENDIAN
#define Tag_val(val) (((volatile unsigned char *) (val)) [-1])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((volatile unsigned char *) (hp)) [sizeof(value)-1])
                                                 /* Also an l-value. */
#else
#define Tag_val(val) (((volatile unsigned char *) (val)) [- (int)sizeof(value)])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((volatile unsigned char *) (hp)) [0])
                                                 /* Also an l-value. */
#endif

#define Unsafe_store_tag_val(dst, val) (Tag_val(dst) = val)
/* Currently [Tag_val(dst)] is an lvalue, but in the future we may
   have to break this property by using explicit (relaxed) atomics to
   avoid undefined behaviors. [Unsafe_store_tag_val(dst, val)] is
   provided to avoid direct uses of [Tag_val(dst)] on the left of an
   assignment. The use of [Unsafe] emphasizes that the function
   may result in unsafe data races in a concurrent setting. */

/* The lowest tag for blocks containing no value. */
#define No_scan_tag 251


/* 1- If tag < No_scan_tag : a tuple of fields.  */

/* Pointer to the first field. */
#define Op_val(x) ((value *) (x))
#define Op_atomic_val(x) ((atomic_value *) (x))
/* Fields are numbered from 0. */
#define Field(x, i) (((volatile value *)(x)) [i]) /* Also an l-value. */

/* NOTE: [Forward_tag] and [Infix_tag] must be just under
   [No_scan_tag], with [Infix_tag] the lower one.
   See [oldify_one] in minor_gc.c for more details.

   NOTE: Update stdlib/obj.ml whenever you change the tags.
 */

/* Forward_tag: forwarding pointer that the GC may silently shortcut.
   See stdlib/lazy.ml. */
#define Forward_tag 250
#define Forward_val(v) Field(v, 0)
/* FIXME: not immutable once shortcutting is implemented */

/* If tag == Infix_tag : an infix header inside a closure */
/* Infix_tag must be odd so that the infix header is scanned as an integer */
/* Infix_tag must be 1 modulo 2 and infix headers can only occur in blocks
   with tag Closure_tag (see compact.c). */

#define Infix_tag 249
#define Infix_offset_hd(hd) (Bosize_hd(hd))
#define Infix_offset_val(v) Infix_offset_hd(Hd_val(v))

/* Another special case: objects */
#define Object_tag 248
#define Class_val(val) Field((val), 0)
#define Oid_val(val) Long_val(Field((val), 1))
/* Allow the bytecode linker to include mlvalues.h without the primitive
   declarations. */
#ifndef CAML_INTERNALS_NO_PRIM_DECLARATIONS
CAMLextern value caml_get_public_method (value obj, value tag);
/* Called as:
   caml_callback(caml_get_public_method(obj, caml_hash_variant(name)), obj) */
/* caml_get_public_method returns 0 if tag not in the table.
   Note however that tags being hashed, same tag does not necessarily mean
   same method name. */
#endif

Caml_inline value Val_ptr(void* p)
{
  CAMLassert(((value)p & 1) == 0);
  return (value)p + 1;
}
Caml_inline void* Ptr_val(value val)
{
  CAMLassert(val & 1);
  return (void*)(val - 1);
}

/* Special case of tuples of fields: closures */
#define Closure_tag 247
#define Code_val(val) (((code_t *) (val)) [0])     /* Also an l-value. */
#define Closinfo_val(val) Field((val), 1)          /* Arity and start env */
/* In the closure info field, the top 8 bits are the arity (signed).
   The low bit is set to one, to look like an integer.
   The remaining bits are the field number for the first word of the
   environment, or, in other words, the offset (in words) from the closure
   to the environment part. */
#ifdef ARCH_SIXTYFOUR
#define Arity_closinfo(info) ((intnat)(info) >> 56)
#define Start_env_closinfo(info) (((uintnat)(info) << 8) >> 9)
#define Make_closinfo(arity,delta) \
  (((uintnat)(arity) << 56) + ((uintnat)(delta) << 1) + 1)
#else
#define Arity_closinfo(info) ((intnat)(info) >> 24)
#define Start_env_closinfo(info) (((uintnat)(info) << 8) >> 9)
#define Make_closinfo(arity,delta) \
  (((uintnat)(arity) << 24) + ((uintnat)(delta) << 1) + 1)
#endif

/* This tag is used (with Forcing_tag & Forward_tag) to implement lazy values.
   See major_gc.c and stdlib/lazy.ml. */
#define Lazy_tag 246

/* Tag used for continuations (see fiber.c) */
#define Cont_tag 245

/* This tag is used (with Lazy_tag & Forward_tag) to implement lazy values.
 * See major_gc.c and stdlib/lazy.ml. */
#define Forcing_tag 244

/* Another special case: variants */
CAMLextern value caml_hash_variant(char const * tag);

/* 2- If tag >= No_scan_tag : a sequence of bytes. */

/* Pointer to the first byte */
#define Bp_val(v) ((char *) (v))
#define Val_bp(p) ((value) (p))
/* Bytes are numbered from 0. */
#define Byte(x, i) (((char *) (x)) [i])            /* Also an l-value. */
#define Byte_u(x, i) (((unsigned char *) (x)) [i]) /* Also an l-value. */

/* Abstract things.  Their contents is not traced by the GC; therefore
   they must not contain any [value]. Must have odd number so that
   headers with this tag cannot be mistaken for pointers. Previously
   used in caml_obj_truncate for a header of the truncated tail of the
   object.
*/
#define Abstract_tag 251
#define Data_abstract_val(v) ((void*) Op_val(v))

/* Strings. */
#define String_tag 252
#define String_val(x) ((const char *) Bp_val(x))
#define Bytes_val(x) ((unsigned char *) Bp_val(x))
CAMLextern mlsize_t caml_string_length (value);   /* size in bytes */
CAMLextern int caml_string_is_c_safe (value);
  /* true if string contains no '\0' null characters */

/* Floating-point numbers. */
#define Double_tag 253
#define Double_wosize ((sizeof(double) / sizeof(value)))
#ifndef ARCH_ALIGN_DOUBLE
#define Double_val(v) (* (double *)(v))
#define Store_double_val(v,d) (* (double *)(v) = (d))
#else
CAMLextern double caml_Double_val (value);
CAMLextern void caml_Store_double_val (value,double);
#define Double_val(v) caml_Double_val(v)
#define Store_double_val(v,d) caml_Store_double_val(v,d)
#endif

/* Arrays of floating-point numbers. */
#define Double_array_tag 254

/* The [_flat_field] macros are for [floatarray] values and float-only records.
*/
#define Double_flat_field(v,i) Double_val((value)((volatile double *)(v) + (i)))
#define Store_double_flat_field(v,i,d) do{ \
  mlsize_t caml__temp_i = (i); \
  double caml__temp_d = (d); \
  Store_double_val((value)((double *) (v) + caml__temp_i), caml__temp_d); \
}while(0)

/* The [_array_field] macros are for [float array]. */
#ifdef FLAT_FLOAT_ARRAY
  #define Double_array_field(v,i) Double_flat_field(v,i)
  #define Store_double_array_field(v,i,d) Store_double_flat_field(v,i,d)
#else
  #define Double_array_field(v,i) Double_val (Field(v,i))
  CAMLextern void caml_Store_double_array_field (value, mlsize_t, double);
  #define Store_double_array_field(v,i,d) caml_Store_double_array_field (v,i,d)
#endif

/* The old [_field] macros are for backward compatibility only.
   They work with [floatarray], float-only records, and [float array]. */
#ifdef FLAT_FLOAT_ARRAY
  #define Double_field(v,i) Double_flat_field(v,i)
  #define Store_double_field(v,i,d) Store_double_flat_field(v,i,d)
#else
  Caml_inline double Double_field (value v, mlsize_t i) {
    if (Tag_val (v) == Double_array_tag){
      return Double_flat_field (v, i);
    }else{
      return Double_array_field (v, i);
    }
  }
  Caml_inline void Store_double_field (value v, mlsize_t i, double d) {
    if (Tag_val (v) == Double_array_tag){
      Store_double_flat_field (v, i, d);
    }else{
      Store_double_array_field (v, i, d);
    }
  }
#endif /* FLAT_FLOAT_ARRAY */

CAMLextern mlsize_t caml_array_length (value);   /* size in items */
CAMLextern int caml_is_double_array (value);   /* 0 is false, 1 is true */


/* Custom blocks.  They contain a pointer to a "method suite"
   of functions (for finalization, comparison, hashing, etc)
   followed by raw data.  The contents of custom blocks is not traced by
   the GC; therefore, they must not contain any [value].
   See [custom.h] for operations on method suites. */
#define Custom_tag 255
#define Data_custom_val(v) ((void *) (Op_val(v) + 1))
struct custom_operations;       /* defined in [custom.h] */

/* Int32.t, Int64.t and Nativeint.t are represented as custom blocks. */

#define Int32_val(v) (*((int32_t *) Data_custom_val(v)))
#define Nativeint_val(v) (*((intnat *) Data_custom_val(v)))
#ifndef ARCH_ALIGN_INT64
#define Int64_val(v) (*((int64_t *) Data_custom_val(v)))
#else
CAMLextern int64_t caml_Int64_val(value v);
#define Int64_val(v) caml_Int64_val(v)
#endif

/* 3- Atoms are 0-tuples.  They are statically allocated once and for all. */

CAMLextern value caml_atom(tag_t);
#define Atom(tag) caml_atom(tag)

/* Booleans are integers 0 or 1 */

#define Val_bool(x) Val_int((x) != 0)
#ifdef __cplusplus
#define Bool_val(x) ((bool) Int_val(x))
#else
#define Bool_val(x) Int_val(x)
#endif
#define Val_false Val_int(0)
#define Val_true Val_int(1)
#define Val_not(x) (Val_false + Val_true - (x))

/* The unit value is 0 (tagged) */

#define Val_unit Val_int(0)

/* List constructors */
#define Val_emptylist Val_int(0)
#define Tag_cons 0

/* Option constructors */

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Tag_some 0
#define Is_none(v) ((v) == Val_none)
#define Is_some(v) Is_block(v)

/* Allow the bytecode linker to include mlvalues.h without the primitive
   declarations. */
#ifndef CAML_INTERNALS_NO_PRIM_DECLARATIONS
CAMLextern value caml_set_oo_id(value obj);
#endif

/* Header for out-of-heap blocks. */

#define Caml_out_of_heap_header_with_reserved(wosize, tag, reserved)   \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                        \
       ((header_t) (Hd_reserved(reserved))                             \
                    + ((header_t) (wosize) << HEADER_WOSIZE_SHIFT)     \
                    + (3 << HEADER_COLOR_SHIFT) /* [NOT_MARKABLE] */   \
                    + (tag_t) (tag)))

#define Caml_out_of_heap_header(wosize, tag)                           \
        Caml_out_of_heap_header_with_reserved(wosize, tag, 0)


/* Obsolete -- suppport for unsafe encoded exceptions.

   Before caml_result was available, we used an unsafe encoding of it
   into the 'value' type, where encoded exceptions have their second
   bit set. These encoded exceptions are invalid values and must not
   be seen by the garbage collector. This is unsafe, and the
   C type-checker does not help. We strongly recommend using the
   caml_result type above instead. It is GC-safe and more
   type-safe. */

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

#ifdef __cplusplus
}
#endif

#endif /* CAML_MLVALUES_H */
