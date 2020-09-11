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

#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/misc.h"

/* Allocation of bigarrays for memory-mapped files.
   This is the OS-independent part of [mmap.c]. */

extern void caml_ba_unmap_file(void *, uintnat);

static void caml_ba_mapped_finalize(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  CAMLassert((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_MAPPED_FILE);
  if (b->proxy == NULL) {
    caml_ba_unmap_file(b->data, caml_ba_byte_size(b));
  } else {
    if (-- b->proxy->refcount == 0) {
      caml_ba_unmap_file(b->proxy->data, b->proxy->size);
      free(b->proxy);
    }
  }
}

/* Operation table for bigarrays representing memory-mapped files.
   Only the finalization method differs from regular bigarrays. */

static struct custom_operations caml_ba_mapped_ops = {
  "_bigarray",
  caml_ba_mapped_finalize,
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  caml_ba_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* [caml_unix_mapped_alloc] allocates a new bigarray object in the heap
   corresponding to a memory-mapped file. */

CAMLexport value
caml_unix_mapped_alloc(int flags, int num_dims, void * data, intnat * dim)
{
  uintnat asize;
  int i;
  value res;
  struct caml_ba_array * b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  CAMLassert(num_dims >= 0 && num_dims <= CAML_BA_MAX_NUM_DIMS);
  CAMLassert((flags & CAML_BA_KIND_MASK) <= CAML_BA_CHAR);
  for (i = 0; i < num_dims; i++) dimcopy[i] = dim[i];
  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  res = caml_alloc_custom(&caml_ba_mapped_ops, asize, 0, 1);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags | CAML_BA_MAPPED_FILE;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dimcopy[i];
  return res;
}
