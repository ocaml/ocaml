/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_BIGARRAY_H
#define CAML_BIGARRAY_H

#include "config.h"
#include "mlvalues.h"
#include "camlatomic.h"

typedef signed char caml_ba_int8;
typedef unsigned char caml_ba_uint8;
typedef int16_t caml_ba_int16;
typedef uint16_t caml_ba_uint16;

#define CAML_BA_MAX_NUM_DIMS 16

enum caml_ba_kind {
  CAML_BA_FLOAT32,             /* Single-precision floats */
  CAML_BA_FLOAT64,             /* Double-precision floats */
  CAML_BA_SINT8,               /* Signed 8-bit integers */
  CAML_BA_UINT8,               /* Unsigned 8-bit integers */
  CAML_BA_SINT16,              /* Signed 16-bit integers */
  CAML_BA_UINT16,              /* Unsigned 16-bit integers */
  CAML_BA_INT32,               /* Signed 32-bit integers */
  CAML_BA_INT64,               /* Signed 64-bit integers */
  CAML_BA_CAML_INT,            /* OCaml-style integers (signed 31 or 63 bits) */
  CAML_BA_NATIVE_INT,        /* Platform-native long integers (32 or 64 bits) */
  CAML_BA_COMPLEX32,           /* Single-precision complex */
  CAML_BA_COMPLEX64,           /* Double-precision complex */
  CAML_BA_CHAR,                /* Characters */
  CAML_BA_FLOAT16,             /* Half-precision floats */
  CAML_BA_FIRST_UNIMPLEMENTED_KIND,
};
#define CAML_BA_KIND_MASK 0xFF /* Mask for kind in flags field */

#define Caml_ba_kind_val(v) Int_val(v)

#define Val_caml_ba_kind(k) Val_int(k)

enum caml_ba_layout {
  CAML_BA_C_LAYOUT = 0,           /* Row major, indices start at 0 */
  CAML_BA_FORTRAN_LAYOUT = 0x100, /* Column major, indices start at 1 */
};
#define CAML_BA_LAYOUT_SHIFT 8    /* Bit offset of layout flag */
#define CAML_BA_LAYOUT_MASK 0x100 /* Mask for layout in flags field */

#define Caml_ba_layout_val(v) (Int_val(v) << CAML_BA_LAYOUT_SHIFT)

#define Val_caml_ba_layout(l) Val_int(l >> CAML_BA_LAYOUT_SHIFT)

enum caml_ba_managed {
  CAML_BA_EXTERNAL = 0,        /* Data is not allocated by OCaml */
  CAML_BA_MANAGED = 0x200,     /* Data is allocated by OCaml */
  CAML_BA_MAPPED_FILE = 0x400, /* Data is a memory mapped file */
};
#define CAML_BA_MANAGED_MASK 0x600 /* Mask for "managed" bits in flags field */

enum caml_ba_subarray {
  CAML_BA_SUBARRAY = 0x800     /* Data is shared with another bigarray */
};

struct caml_ba_proxy {
  atomic_uintnat refcount;      /* Reference count */
  void * data;                  /* Pointer to base of actual data */
  uintnat size;                 /* Size of data in bytes (if mapped file) */
};

struct caml_ba_array {
  void * data;                /* Pointer to raw data */
  intnat num_dims;            /* Number of dimensions */
  intnat flags;  /* Kind of element array + memory layout + allocation status */
  struct caml_ba_proxy * proxy; /* The proxy for sub-arrays, or NULL */
  intnat dim[/* num_dims */]; /* Size in each dimension */
};

/* Size of struct caml_ba_array, in bytes, without [dim] array */
#define SIZEOF_BA_ARRAY sizeof(struct caml_ba_array)

#define Caml_ba_array_val(v) ((struct caml_ba_array *) Data_custom_val(v))

#define Caml_ba_data_val(v) (Caml_ba_array_val(v)->data)

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern value
    caml_ba_alloc(int flags, int num_dims, void * data, intnat * dim);
CAMLextern value caml_ba_alloc_dims(int flags, int num_dims, void * data,
                                 ... /*dimensions, with type intnat */);
CAMLextern uintnat caml_ba_byte_size(struct caml_ba_array * b);
CAMLextern uintnat caml_ba_num_elts(struct caml_ba_array * b);

#ifdef __cplusplus
}
#endif

#ifdef CAML_INTERNALS

CAMLextern const int caml_ba_element_size[];
CAMLextern void caml_ba_finalize(value v);
CAMLextern int caml_ba_compare(value v1, value v2);
CAMLextern intnat caml_ba_hash(value v);
CAMLextern void caml_ba_serialize(value, uintnat *, uintnat *);
CAMLextern uintnat caml_ba_deserialize(void * dst);

#endif  /* CAML_INTERNALS */

#endif /* CAML_BIGARRAY_H */
