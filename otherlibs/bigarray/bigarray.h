/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _bigarray_
#define _bigarray_


#include "mlvalues.h"

#define MAX_NUM_DIMS 16

enum caml_bigarray_kind {
  BIGARRAY_FLOAT32,             /* Single-precision floats */
  BIGARRAY_FLOAT64,             /* Double-precision floats */
  BIGARRAY_SINT8,               /* Signed 8-bit integers */
  BIGARRAY_UINT8,               /* Unsigned 8-bit integers */
  BIGARRAY_SINT16,              /* Signed 16-bit integers */
  BIGARRAY_UINT16,              /* Unsigned 16-bit integers */
  BIGARRAY_INT32,               /* Signed 32-bit integers */
  BIGARRAY_INT64,               /* Signed 64-bit integers */
  BIGARRAY_CAML_INT,            /* Caml-style integers (signed 31 or 63 bits) */
  BIGARRAY_NATIVE_INT,      /* Platform-native long integers (32 or 64 bits) */
  BIGARRAY_COMPLEX32,           /* Single-precision complex */
  BIGARRAY_COMPLEX64,           /* Double-precision complex */
  BIGARRAY_KIND_MASK = 0xFF     /* Mask for kind in flags field */
};

enum caml_bigarray_layout {
  BIGARRAY_C_LAYOUT = 0,           /* Row major, indices start at 0 */
  BIGARRAY_FORTRAN_LAYOUT = 0x100, /* Column major, indices start at 1 */
  BIGARRAY_LAYOUT_MASK = 0x100  /* Mask for layout in flags field */
};

enum caml_bigarray_managed {
  BIGARRAY_EXTERNAL = 0,        /* Data is not allocated by Caml */
  BIGARRAY_MANAGED = 0x200,     /* Data is allocated by Caml */
  BIGARRAY_MAPPED_FILE = 0x400, /* Data is a memory mapped file */
  BIGARRAY_MANAGED_MASK = 0x600 /* Mask for "managed" bits in flags field */
};

struct caml_bigarray_proxy {
  long refcount;                /* Reference count */
  void * data;                  /* Pointer to base of actual data */
  unsigned long size;           /* Size of data in bytes (if mapped file) */
};

struct caml_bigarray {
  void * data;                /* Pointer to raw data */
  long num_dims;              /* Number of dimensions */
  long flags;   /* Kind of element array + memory layout + allocation status */
  struct caml_bigarray_proxy * proxy; /* The proxy for sub-arrays, or NULL */
  long dim[1] /*[num_dims]*/; /* Size in each dimension */
};

#define Bigarray_val(v) ((struct caml_bigarray *) Data_custom_val(v))

#define Data_bigarray_val(v) (Bigarray_val(v)->data)

#if defined(IN_OCAML_BIGARRAY)
#define CAMLBAextern CAMLexport
#else
#define CAMLBAextern CAMLextern
#endif

CAMLBAextern value alloc_bigarray(int flags, int num_dims, void * data, long * dim);
CAMLBAextern value alloc_bigarray_dims(int flags, int num_dims, void * data,
                                 ... /*dimensions, with type long */);

#endif
