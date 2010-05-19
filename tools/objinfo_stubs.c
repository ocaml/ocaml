/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*        Mehdi Dogguy, PPS laboratory, University Paris Diderot       */
/*                                                                     */
/*  Copyright 2010 Mehdi Dogguy.  Used and distributed as part of      */
/*  Objective Caml by permission from the author.   This file is       */
/*  distributed under the terms of the Q Public License version 1.0.   */
/***********************************************************************/

#include "../config/s.h"
#include "../byterun/mlvalues.h"
#include "../byterun/alloc.h"

#ifdef HAS_LIBBFD
#include <stdlib.h>
#include <string.h>
#include <bfd.h>

static file_ptr get_cmxs_offset (char *file)
{
  file_ptr result = -1;
  bfd *fd;
  asection *sec;
  file_ptr offset;
  long st_size;
  asymbol ** symbol_table;
  long sym_count, i;

  fd = bfd_openr(file, "default");
  if (!fd) return -1;

  do {
    if (! bfd_check_format (fd, bfd_object)) break;

    sec = bfd_get_section_by_name(fd, ".data");
    if (! sec) break;

    offset = sec->filepos;
    st_size = bfd_get_dynamic_symtab_upper_bound (fd);
    if (st_size <= 0) break;

    symbol_table = malloc(st_size);
    if (! symbol_table) break;

    sym_count = bfd_canonicalize_dynamic_symtab (fd, symbol_table);
    for (i = 0; i < sym_count; i++) {
      if (strcmp(symbol_table[i]->name, "caml_plugin_header") == 0) {
        result = offset + symbol_table[i]->value;
        break;
      }
    }
    free(symbol_table);
  }
  while (0);
  bfd_close(fd);
  return result;
}

#endif

CAMLprim value caml_get_cmxs_offset(value file)
{
#ifdef HAS_LIBBFD
  return caml_copy_int64(get_cmxs_offset(String_val(file)));
#else
  return caml_copy_int64(-2);
#endif
}
