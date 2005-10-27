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

#include <stddef.h>
#include <string.h>
#include "bigarray.h"
#include "custom.h"
#include "fail.h"
#include "mlvalues.h"
#include "sys.h"

extern int bigarray_element_size[];  /* from bigarray_stubs.c */

#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#endif

#if defined(HAS_MMAP)

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

CAMLprim value bigarray_map_file(value vfd, value vkind, value vlayout,
                                 value vshared, value vdim)
{
  int fd, flags, major_dim, shared;
  intnat num_dims, i;
  intnat dim[MAX_NUM_DIMS];
  intnat currpos, file_size;
  uintnat array_size;
  char c;
  void * addr;

  fd = Int_val(vfd);
  flags = Int_val(vkind) | Int_val(vlayout);
  num_dims = Wosize_val(vdim);
  major_dim = flags & BIGARRAY_FORTRAN_LAYOUT ? num_dims - 1 : 0;
  /* Extract dimensions from Caml array */
  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > MAX_NUM_DIMS)
    invalid_argument("Bigarray.mmap: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] == -1 && i == major_dim) continue;
    if (dim[i] < 0 || dim[i] > 0x7FFFFFFFL)
      invalid_argument("Bigarray.create: negative dimension");
  }
  /* Determine file size */
  currpos = lseek(fd, 0, SEEK_CUR);
  if (currpos == -1) sys_error(NO_ARG);
  file_size = lseek(fd, 0, SEEK_END);
  if (file_size == -1) sys_error(NO_ARG);
  /* Determine array size in bytes (or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = bigarray_element_size[flags & BIGARRAY_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    if (dim[i] != -1) array_size *= dim[i];
  /* Check if the first/last dimension is unknown */
  if (dim[major_dim] == -1) {
    /* Determine first/last dimension from file size */
    if ((uintnat) file_size % array_size != 0)
      failwith("Bigarray.mmap: file size doesn't match array dimensions");
    dim[major_dim] = (uintnat) file_size / array_size;
    array_size = file_size;
  } else {
    /* Check that file is large enough, and grow it otherwise */
    if (file_size < array_size) {
      if (lseek(fd, array_size - 1, SEEK_SET) == -1) sys_error(NO_ARG);
      c = 0;
      if (write(fd, &c, 1) != 1) sys_error(NO_ARG);
    }
  }
  /* Restore original file position */
  lseek(fd, currpos, SEEK_SET);
  /* Do the mmap */
  shared = Bool_val(vshared) ? MAP_SHARED : MAP_PRIVATE;
  addr = mmap(NULL, array_size, PROT_READ | PROT_WRITE, shared, fd, 0);
  if (addr == (void *) MAP_FAILED) sys_error(NO_ARG);
  /* Build and return the Caml bigarray */
  return alloc_bigarray(flags | BIGARRAY_MAPPED_FILE, num_dims, addr, dim);
}

#else

value bigarray_map_file(value vfd, value vkind, value vlayout,
                        value vshared, value vdim)
{
  invalid_argument("Bigarray.map_file: not supported");
  return Val_unit;
}

#endif


void bigarray_unmap_file(void * addr, uintnat len)
{
#if defined(HAS_MMAP)
  munmap(addr, len);
#endif
}
