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
#include "io.h"
#include "mlvalues.h"
#include "sys.h"
#include "signals.h"

extern int caml_ba_element_size[];  /* from bigarray_stubs.c */

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

CAMLprim value caml_ba_map_file(value vfd, value vkind, value vlayout,
                                value vshared, value vdim, value vstart)
{
  int fd, flags, major_dim, shared;
  intnat num_dims, i;
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  file_offset currpos, startpos, file_size, data_size;
  uintnat array_size, page, delta;
  char c;
  void * addr;

  fd = Int_val(vfd);
  flags = Int_val(vkind) | Int_val(vlayout);
  startpos = File_offset_val(vstart);
  num_dims = Wosize_val(vdim);
  major_dim = flags & CAML_BA_FORTRAN_LAYOUT ? num_dims - 1 : 0;
  /* Extract dimensions from Caml array */
  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Bigarray.mmap: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] == -1 && i == major_dim) continue;
    if (dim[i] < 0)
      caml_invalid_argument("Bigarray.create: negative dimension");
  }
  /* Determine file size */
  caml_enter_blocking_section();
  currpos = lseek(fd, 0, SEEK_CUR);
  if (currpos == -1) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  file_size = lseek(fd, 0, SEEK_END);
  if (file_size == -1) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  /* Determine array size in bytes (or size of array without the major
     dimension if that dimension wasn't specified) */
  array_size = caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  for (i = 0; i < num_dims; i++)
    if (dim[i] != -1) array_size *= dim[i];
  /* Check if the major dimension is unknown */
  if (dim[major_dim] == -1) {
    /* Determine major dimension from file size */
    if (file_size < startpos) {
      caml_leave_blocking_section();
      caml_failwith("Bigarray.mmap: file position exceeds file size");
    }
    data_size = file_size - startpos;
    dim[major_dim] = (uintnat) (data_size / array_size);
    array_size = dim[major_dim] * array_size;
    if (array_size != data_size) {
      caml_leave_blocking_section();
      caml_failwith("Bigarray.mmap: file size doesn't match array dimensions");
    }
  } else {
    /* Check that file is large enough, and grow it otherwise */
    if (file_size < startpos + array_size) {
      if (lseek(fd, startpos + array_size - 1, SEEK_SET) == -1) {
        caml_leave_blocking_section();
        caml_sys_error(NO_ARG);
      }
      c = 0;
      if (write(fd, &c, 1) != 1) {
        caml_leave_blocking_section();
        caml_sys_error(NO_ARG);
      }
    }
  }
  /* Restore original file position */
  lseek(fd, currpos, SEEK_SET);
  /* Determine offset so that the mapping starts at the given file pos */
  page = getpagesize();
  delta = (uintnat) (startpos % page);
  /* Do the mmap */
  shared = Bool_val(vshared) ? MAP_SHARED : MAP_PRIVATE;
  addr = mmap(NULL, array_size + delta, PROT_READ | PROT_WRITE,
              shared, fd, startpos - delta);
  caml_leave_blocking_section();
  if (addr == (void *) MAP_FAILED) caml_sys_error(NO_ARG);
  addr = (void *) ((uintnat) addr + delta);
  /* Build and return the Caml bigarray */
  return caml_ba_alloc(flags | CAML_BA_MAPPED_FILE, num_dims, addr, dim);
}

#else

value caml_ba_map_file(value vfd, value vkind, value vlayout,
                       value vshared, value vdim, value vpos)
{
  caml_invalid_argument("Bigarray.map_file: not supported");
  return Val_unit;
}

#endif

CAMLprim value caml_ba_map_file_bytecode(value * argv, int argn)
{
  return caml_ba_map_file(argv[0], argv[1], argv[2],
                          argv[3], argv[4], argv[5]);
}

void caml_ba_unmap_file(void * addr, uintnat len)
{
#if defined(HAS_MMAP)
  uintnat page = getpagesize();
  uintnat delta = (uintnat) addr % page;
  munmap((void *)((uintnat)addr - delta), len + delta);
#endif
}
