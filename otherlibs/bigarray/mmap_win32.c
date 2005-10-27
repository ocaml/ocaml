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
#include <stdio.h>
#include <string.h>
#include "bigarray.h"
#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "mlvalues.h"
#include "sys.h"
#include "unixsupport.h"

/* TODO: handle mappings larger than 2^32 bytes on Win64 */

extern int bigarray_element_size[];  /* from bigarray_stubs.c */

static void bigarray_sys_error(void);

CAMLprim value bigarray_map_file(value vfd, value vkind, value vlayout,
                                 value vshared, value vdim)
{
  HANDLE fd, fmap;
  int flags, major_dim, mode, perm;
  intnat num_dims, i;
  intnat dim[MAX_NUM_DIMS];
  DWORD currpos, file_size;
  uintnat array_size;
  char c;
  void * addr;

  fd = Handle_val(vfd);
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
  currpos = SetFilePointer(fd, 0, NULL, FILE_CURRENT);
  if (currpos == INVALID_SET_FILE_POINTER) bigarray_sys_error();
  file_size = SetFilePointer(fd, 0, NULL, FILE_END);
  if (file_size == INVALID_SET_FILE_POINTER) bigarray_sys_error();
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
  }
  /* Restore original file position */
  SetFilePointer(fd, currpos, NULL, FILE_BEGIN);
  /* Create the file mapping */
  if (Bool_val(vshared)) {
    perm = PAGE_READWRITE;
    mode = FILE_MAP_WRITE;
  } else {
    perm = PAGE_READONLY;       /* doesn't work under Win98 */
    mode = FILE_MAP_COPY;
  }
  fmap = CreateFileMapping(fd, NULL, perm, 0, array_size, NULL);
  if (fmap == NULL) bigarray_sys_error();
  /* Map the mapping in memory */
  addr = MapViewOfFile(fmap, mode, 0, 0, array_size);
  if (addr == NULL) bigarray_sys_error();
  /* Close the file mapping */
  CloseHandle(fmap);
  /* Build and return the Caml bigarray */
  return alloc_bigarray(flags | BIGARRAY_MAPPED_FILE, num_dims, addr, dim);
}

void bigarray_unmap_file(void * addr, uintnat len)
{
  UnmapViewOfFile(addr);
}

static void bigarray_sys_error(void)
{
  char buffer[512];
  DWORD errnum;
  
  errnum = GetLastError();
  if (!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
                     NULL,
                     errnum,
                     0,
                     buffer,
                     sizeof(buffer),
                     NULL))
    sprintf(buffer, "Unknown error %ld\n", errnum);
  raise_sys_error(copy_string(buffer));
}
