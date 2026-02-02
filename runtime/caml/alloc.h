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

#ifndef CAML_ALLOC_H
#define CAML_ALLOC_H

#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/* It is guaranteed that these allocation functions will not trigger
   any OCaml callback such as finalizers or signal handlers. */

/* Allocates a block with given size with tag.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc (mlsize_t, tag_t);

/* Allocates a block of size 1 with given tag,
   and sets its only field to the given value.   
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_1(tag_t, value);

/* Allocates a block of size 2 with given tag,
   and sets its two field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_2(tag_t, value, value);

/* Allocates a block of size 3 with given tag,
   and sets its three field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_3(tag_t, value, value, value);

/* Allocates a block of size 4 with given tag,
   and sets its four field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_4(tag_t, value, value, value, value);

/* Allocates a block of size 5 with given tag,
   and sets its 5 field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_5(tag_t, value, value, value, value,
                              value);

/* Allocates a block of size 6 with given tag,
   and sets its six field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_6(tag_t, value, value, value, value,
                              value, value);

/* Allocates a block of size 7 with given tag,
   and sets its seven field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_7(tag_t, value, value, value, value,
                              value, value, value);

/* Allocates a block of size 8 with given tag,
   and sets its eight field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_8(tag_t, value, value, value, value,
                              value, value, value, value);

/* Allocates a block of size 9 with given tag,
   and sets its nine field to the given values.
   Guaranteed to not trigger any OCaml callback such as
   finalizers or signal handlers. */
CAMLextern value caml_alloc_9(tag_t, value, value, value, value,
                              value, value, value, value, value);

/* `caml_alloc_small(n, t)` returns a fresh small block of size
   `n ≤ Max_young_wosize` words, with tag `t`.
   If this block is a structured block (i.e. if `t < No_scan_tag`),
   then the fields of the block (initially containing garbage)
   must be initialized with legal values
   (using direct assignment to the fields of the block)
   before the next allocation. */
CAMLextern value caml_alloc_small (mlsize_t, tag_t);
CAMLextern value caml_alloc_shr_check_gc (mlsize_t, tag_t);

/* `caml_alloc_tuple(n)` returns a fresh block of size n words, with tag `0`. */
CAMLextern value caml_alloc_tuple (mlsize_t);

/* `caml_alloc_float_array(n)` allocates an array of
   floating point numbers of size n. The array initially
   contains uninitialized values. */
CAMLextern value caml_alloc_float_array(mlsize_t len);

/* `caml_alloc_string(n)` returns a byte sequence (or string)
   value of length n bytes.
   The sequence initially contains uninitialized bytes.*/
CAMLextern value caml_alloc_string (mlsize_t len);

/* `caml_alloc_initialized_string(n, p)` returns a byte sequence
   (or string) value of length n bytes.
   The value is initialized from the `n` bytes starting at address `p`. */
CAMLextern value caml_alloc_initialized_string (mlsize_t len, const char *);

/* `caml_copy_string(s)` allocates and returns a string or byte sequence value
   containing a copy of the null-terminated C string `s` (a `char *`). */
CAMLextern value caml_copy_string(char const *);

/* `caml_copy_string_array(p)` allocates an array of strings or byte sequences,
   copied from the pointer to a string array `p` (a `char **`).
   `p` must be a null-terminated array of pointers, each of which must
   point to a null-terminated C string. */
CAMLextern value caml_copy_string_array (char const * const*);

/* `caml_copy_double(d)` returns a
   floating-point value initialized with the `double` `d`. */
CAMLextern value caml_copy_double (double);

/* `caml_copy_int32(i)` return a value of OCaml type `int32`
   initialized with the integer `i`. */
CAMLextern value caml_copy_int32(int32_t); /* defined in [ints.c] */

/* `caml_copy_int64(i)` return a value of OCaml type `int64`
   initialized with the integer `i`. */
CAMLextern value caml_copy_int64 (int64_t);       /* defined in [ints.c] */

/* `caml_copy_nativeint(i)` return a value of OCaml type `nativeint`
   initialized with the integer `i`. */
CAMLextern value caml_copy_nativeint (intnat);  /* defined in [ints.c] */

/* `caml_alloc_array(f, a)` allocates an array of values,
   calling function `f` over each element of the input array `a`
   to transform it into a value. The array `a` is an array of pointers
   terminated by the null pointer.
   The function `f` receives each pointer as argument, and returns a value.
   The zero-tagged block returned by `alloc_array(f, a)`
   is filled with the values returned by the successive calls to `f`.
   (This function must not be used to build an array of
   floating-point numbers.) */
CAMLextern value caml_alloc_array (value (*funct) (char const *),
                                   char const * const * array);

CAMLextern value caml_alloc_sprintf(const char * format, ...)
#if __has_attribute(format) || defined(__GNUC__)
  __attribute__ ((format (printf, 1, 2)))
#endif
;

/* `caml_alloc_some(v)` allocates a block representing `Some(v)`. */
CAMLextern value caml_alloc_some(value);

typedef void (*final_fun)(value);
CAMLextern value caml_alloc_final (mlsize_t, /*size in words*/
                                   final_fun, /*finalization function*/
                                   mlsize_t, /*resources consumed*/
                                   mlsize_t  /*max resources*/);

CAMLextern int caml_convert_flag_list (value, const int *);

/* Convenience functions to deal with unboxable types. */

/* `caml_alloc_unboxed(v)` returns the value
   (of any unboxed type) whose field is the value `v`. */
Caml_inline value caml_alloc_unboxed (value arg) { return arg; }

/* `caml_alloc_boxed(v)` allocates and returns a value
   (of any boxed type) whose field is the value `v`. */
Caml_inline value caml_alloc_boxed (value arg) {
  value result = caml_alloc_small (1, 0);
  Field (result, 0) = arg;
  return result;
}

Caml_inline value caml_field_unboxed (value arg) { return arg; }
Caml_inline value caml_field_boxed (value arg) { return Field (arg, 0); }

/* `caml_alloc_unboxable(v)` calls either `caml_alloc_unboxed` or
   `caml_alloc_boxed` according to the default representation of unboxable
   types in the current version of OCaml.
   Currently, unannotated unboxable types are boxed by default.
   (may change in the future) */
#define caml_alloc_unboxable caml_alloc_boxed
#define caml_field_unboxable caml_field_boxed

#ifdef __cplusplus
}
#endif

#endif /* CAML_ALLOC_H */
