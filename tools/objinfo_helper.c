/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*         Mehdi Dogguy, PPS laboratory, University Paris Diderot         */
/*                                                                        */
/*   Copyright 2010 Mehdi Dogguy                                          */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "caml/s.h"
#include <stdio.h>

#ifdef HAS_LIBBFD
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

// PACKAGE: protect against binutils change
//   https://sourceware.org/bugzilla/show_bug.cgi?id=14243
#define PACKAGE "ocamlobjinfo"
#include <bfd.h>
#undef PACKAGE

#define plugin_header_sym (symbol_prefix "caml_plugin_header")

/* Print an error message and exit */
static void error(bfd *fd, char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  vfprintf (stderr, msg, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  if (fd!=NULL) bfd_close(fd);
  exit(2);
}

int main(int argc, char ** argv)
{
  bfd *fd;
  asection *sec;
  file_ptr offset;
  long st_size;
  asymbol ** symbol_table;
  long sym_count, i;

  if (argc != 2)
    error(NULL, "Usage: objinfo_helper <dynamic library>");

  fd = bfd_openr(argv[1], "default");
  if (!fd)
    error(NULL, "Error opening file %s", argv[1]);
  if (! bfd_check_format (fd, bfd_object))
    error(fd, "Error: wrong format");

  sec = bfd_get_section_by_name(fd, ".data");
  if (! sec)
    error(fd, "Error: section .data not found");

  offset = sec->filepos;
  st_size = bfd_get_dynamic_symtab_upper_bound (fd);
  if (st_size <= 0)
    error(fd, "Error: size of section .data unknown");

  symbol_table = malloc(st_size);
  if (! symbol_table)
    error(fd, "Error: out of memory");

  sym_count = bfd_canonicalize_dynamic_symtab (fd, symbol_table);

  for (i = 0; i < sym_count; i++) {
    if (strcmp(symbol_table[i]->name, plugin_header_sym) == 0) {
      printf("%ld\n", (long) (offset + symbol_table[i]->value));
      bfd_close(fd);
      return 0;
    }
  }

  error(fd, "Error: missing symbol %s", plugin_header_sym);
}

#else

int main(int argc, char ** argv)
{
  fprintf(stderr,"BFD library unavailable, cannot print info on .cmxs files\n");
  return 2;
}

#endif
