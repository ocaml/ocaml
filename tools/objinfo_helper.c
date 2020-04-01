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

/* We need to refer to a few functions of the BFD library that are */
/* actually defined as macros. We thus define equivalent */
/* functions below */

long get_static_symtab_upper_bound(bfd *fd)
{
  return bfd_get_symtab_upper_bound(fd);
}

long get_dynamic_symtab_upper_bound(bfd *fd)
{
  return bfd_get_dynamic_symtab_upper_bound(fd);
}

long canonicalize_static_symtab(bfd * fd, asymbol **symbolTable)
{
  return bfd_canonicalize_symtab(fd, symbolTable);
}

long canonicalize_dynamic_symtab(bfd * fd, asymbol **symbolTable)
{
  return bfd_canonicalize_dynamic_symtab(fd, symbolTable);
}

typedef struct {
  long (*get_upper_bound)(bfd *);
  long (*canonicalize)(bfd *, asymbol **);
} symTable_ops;

symTable_ops staticSymTable_ops = {
  &get_static_symtab_upper_bound,
  &canonicalize_static_symtab
};

symTable_ops dynamicSymTable_ops = {
  &get_dynamic_symtab_upper_bound,
  &canonicalize_dynamic_symtab
};

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

/* Look for plugin_header_sym in the specified symbol table */
/* Return its address, -1 if not found */
long lookup(bfd* fd, symTable_ops *ops)
{
  long st_size;
  asymbol ** symbol_table;
  long sym_count, i;

  st_size = ops->get_upper_bound (fd);
  if (st_size <= 0) return -1;

  symbol_table = malloc(st_size);
  if (! symbol_table)
    error(fd, "Error: out of memory");

  sym_count = ops->canonicalize (fd, symbol_table);

  for (i = 0; i < sym_count; i++) {
    if (strcmp(symbol_table[i]->name, plugin_header_sym) == 0)
      return symbol_table[i]->value;
  }
  return -1;
}

int main(int argc, char ** argv)
{
  bfd *fd;
  asection *sec;
  file_ptr offset;
  long value;

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

  value = lookup(fd, &dynamicSymTable_ops);

  if (value == -1)
    value = lookup(fd, &staticSymTable_ops);
  bfd_close(fd);

  if (value == -1)
    error(NULL, "Error: missing symbol %s", plugin_header_sym);

  printf("%ld\n", (long) offset + value);
}

#else

int main(int argc, char ** argv)
{
  fprintf(stderr,"BFD library unavailable, cannot print info on .cmxs files\n");
  return 2;
}

#endif
