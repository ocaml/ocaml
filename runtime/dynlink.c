/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Dynamic loading of C primitives. */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef _WIN32
#include <io.h>
#endif
#include "caml/alloc.h"
#include "caml/dynlink.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/prims.h"
#include "caml/signals.h"
#include "caml/intext.h"
#include "caml/startup.h"

#include "build_config.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef NATIVE_CODE

/* The table of primitives */
struct ext_table caml_prim_table;

/* The names of primitives */
struct ext_table caml_prim_name_table;

/* The table of shared libraries currently opened */
static struct ext_table shared_libs;

/* The search path for shared libraries */
struct ext_table caml_shared_libs_path;

/* Look up the given primitive name in the built-in primitive table,
   then in the opened shared libraries (shared_libs) */
static c_primitive lookup_primitive(const char * name)
{
  void * res;

  for (int i = 0; caml_names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, caml_names_of_builtin_cprim[i]) == 0)
      return caml_builtin_cprim[i];
  }
  for (int i = 0; i < shared_libs.size; i++) {
    res = caml_dlsym(shared_libs.contents[i], name);
    if (res != NULL) return (c_primitive) res;
  }
  return NULL;
}

#endif /* NATIVE_CODE */

/* Parse the ld.conf file and add the directories
   listed there to the search path */

#define LD_CONF_NAME T("ld.conf")

/* Return a copy of [path], interpreting explicit-relative paths relative to
   [root]. [root] must not end with a directory separator and is expected to be
   absolute. The result of this function can never be ".", ".." or a path
   beginning "./" or "../". Note that the function does not necessarily
   canonicalise the path. */
static char_os *make_relative_path_absolute(char_os *path, char_os *root)
{
  if (path[0] == '.') {
    if (path[1] == '\0') {
      /* path is exactly "." => return root */
      return caml_stat_strdup_os(root);
    } else if (Is_separator(path[1])) {
      /* path is exactly "./" or begins "./". In both cases, replace the "."
         with root */
      return caml_stat_strconcat_os(2, root, (path + 1));
    } else if (path[1] == '.' && (path[2] == '\0' || Is_separator(path[2]))) {
      /* path is either exactly ".." or begins "../" => prefix it with root
         (which has no trailing separator) */
      return caml_stat_strconcat_os(3, root, CAML_DIR_SEP, path);
    } else {
      /* path is not explicit-relative, but simply begins with a dot
           => return a copy */
      return caml_stat_strdup_os(path);
    }
  } else {
    /* path is not explicit-relative => return a copy */
    return caml_stat_strdup_os(path);
  }
}

CAMLexport char_os * caml_parse_ld_conf(const char_os * stdlib,
                                        struct ext_table *table)
{
  const char_os * const locations[3] = {
    caml_secure_getenv(T("OCAMLLIB")),
    caml_secure_getenv(T("CAMLLIB")),
    stdlib};
  char_os * libroot, * ldconfname, * wconfig, * p, * q;
  char * config;
#ifdef _WIN32
  struct _stati64 st;
#else
  struct stat st;
#endif
  int ldconf, nread;
  size_t length = 0;
  struct ext_table entries;

  /* Use a temporary ext_table to hold the individually-allocated entries */
  caml_ext_table_init(&entries, 8);
  for (int i = 0; i < sizeof(locations) / sizeof(locations[0]); i++) {
    if (locations[i] != NULL) {
      libroot = caml_stat_strdup_os(locations[i]);
      size_t libroot_length = strlen_os(libroot);
      while (libroot_length > 0 && Is_separator(libroot[libroot_length - 1]))
        libroot[--libroot_length] = '\0';
      ldconfname =
        caml_stat_strconcat_os(3, libroot, CAML_DIR_SEP, LD_CONF_NAME);
      if (stat_os(ldconfname, &st) == -1) {
        caml_stat_free(ldconfname);
        caml_stat_free(libroot);
        continue;
      }
      ldconf = open_os(ldconfname, O_RDONLY | O_BINARY, 0);
      if (ldconf == -1)
        caml_fatal_error("cannot read loader config file %s",
                             caml_stat_strdup_of_os(ldconfname));
      config = caml_stat_alloc(st.st_size + 1);
      nread = read(ldconf, config, st.st_size);
      if (nread == -1)
        caml_fatal_error
          ("error while reading loader config file %s",
           caml_stat_strdup_of_os(ldconfname));
      close(ldconf);
      config[nread] = 0;
      wconfig = caml_stat_strdup_to_os(config);
      caml_stat_free(config);
      caml_stat_free(ldconfname);

      p = wconfig;
      while (*p != '\0') {
        for (q = p; *q != '\0' && *q != '\n'; q++) /*nothing*/;
        char_os *r = q;
        if (*q == '\n') {
          r++;
          /* Ignore any trailing CR characters, so that CR*LF is uniformly
             treated as a single LF. */
          while (q > p && *(q - 1) == '\r')
            q--;
        }
        *q = '\0';
        char_os *entry = make_relative_path_absolute(p, libroot);
        length += strlen_os(entry) + 1;
        caml_ext_table_add(&entries, entry);
        p = r;
      }

      caml_stat_free(wconfig);
      caml_stat_free(libroot);
    }
  }

  /* Now concatenate them all and load the search path */
  char_os *result = caml_stat_alloc(length * sizeof(char_os));
  p = result;
  for (int i = 0; i < entries.size; i++) {
    char_os *entry = entries.contents[i];
    length = strlen_os(entry) + 1;
    memcpy(p, entry, length * sizeof(char_os));
    caml_ext_table_add(table, p);
    p += length;
  }
  caml_ext_table_free(&entries, 1);

  return result;
}

/* Exposes caml_parse_ld_conf as a primitive for the bytecode compiler, saving
   the duplication of the logic within the bytecode compiler. */
CAMLprim value caml_dynlink_parse_ld_conf(value vstdlib)
{
  CAMLparam1(vstdlib);
  CAMLlocal2(list, str);

  char_os *stdlib = caml_stat_strdup_to_os(String_val(vstdlib));
  struct ext_table table;
  caml_ext_table_init(&table, 8);
  char_os *tofree = caml_parse_ld_conf(stdlib, &table);
  caml_stat_free(stdlib);

  list = Val_emptylist;
  for (int i = table.size - 1; i >= 0; i--) {
    str = caml_copy_string_of_os(table.contents[i]);
    list = caml_alloc_2(Tag_cons, str, list);
  }

  caml_ext_table_free(&table, 0);
  caml_stat_free(tofree);

  CAMLreturn(list);
}

#ifndef NATIVE_CODE

/* Open the given shared library and add it to shared_libs.
   Abort on error. */
static void open_shared_lib(char_os * name)
{
  char_os * realname;
  char * u8;
  void * handle;

  realname = caml_search_dll_in_path(&caml_shared_libs_path, name);
  u8 = caml_stat_strdup_of_os(realname);
  CAML_GC_MESSAGE(STARTUP, "Loading shared library %s\n", u8);
  caml_stat_free(u8);
  caml_enter_blocking_section();
  handle = caml_dlopen(realname, 1);
  caml_leave_blocking_section();
  if (handle == NULL)
    caml_fatal_error
    (
      "cannot load shared library %s\n"
      "Reason: %s",
      caml_stat_strdup_of_os(name),
      caml_dlerror()
    );
  caml_ext_table_add(&shared_libs, handle);
  caml_stat_free(realname);
}

/* Build the table of primitives, given a search path and a list
   of shared libraries (both 0-separated in a char array).
   Abort the runtime system on error. */
void caml_build_primitive_table(char_os * lib_path,
                                char_os * libs,
                                char * req_prims)
{
  /* Initialize the search path for dynamic libraries:
     - directories specified on the command line with the -I option
     - directories specified in the CAML_LD_LIBRARY_PATH
     - directories specified in the executable
     - directories specified in OCAMLLIB/ld.conf
     - directories specified in CAMLLIB/ld.conf
     - directories specified in the file <stdlib>/ld.conf

     caml_shared_libs_path and caml_prim_name_table are not freed afterwards:
     they may later be used by caml_dynlink_get_bytecode_sections. */
  caml_decompose_path(&caml_shared_libs_path,
                      caml_secure_getenv(T("CAML_LD_LIBRARY_PATH")));
  if (lib_path != NULL)
    for (char_os *p = lib_path; *p != 0; p += strlen_os(p) + 1)
      caml_ext_table_add(&caml_shared_libs_path, p);
  caml_parse_ld_conf(OCAML_STDLIB_DIR, &caml_shared_libs_path);
  /* Open the shared libraries */
  caml_ext_table_init(&shared_libs, 8);
  if (libs != NULL)
    for (char_os *p = libs; *p != 0; p += strlen_os(p) + 1)
      open_shared_lib(p);
  /* Build the primitive table */
  caml_ext_table_init(&caml_prim_table, 0x180);
  caml_ext_table_init(&caml_prim_name_table, 0x180);
  if (req_prims != NULL)
    for (char *q = req_prims; *q != 0; q += strlen(q) + 1) {
      c_primitive prim = lookup_primitive(q);
      if (prim == NULL)
            caml_fatal_error("unknown C primitive `%s'", q);
      caml_ext_table_add(&caml_prim_table, (void *) prim);
      caml_ext_table_add(&caml_prim_name_table, caml_stat_strdup(q));
    }
}

/* Build the table of primitives as a copy of the builtin primitive table.
   Used for executables generated by ocamlc -output-obj. */

void caml_build_primitive_table_builtin(void)
{
  caml_build_primitive_table(NULL, NULL, NULL);
  for (int i = 0; caml_builtin_cprim[i] != 0; i++) {
    caml_ext_table_add(&caml_prim_table, (void *) caml_builtin_cprim[i]);
    caml_ext_table_add(&caml_prim_name_table,
                       caml_stat_strdup(caml_names_of_builtin_cprim[i]));
  }
}

void caml_free_shared_libs(void)
{
  while (shared_libs.size > 0)
    caml_dlclose(shared_libs.contents[--shared_libs.size]);
}

CAMLprim value caml_dynlink_get_bytecode_sections(value unit)
{
  CAMLparam1(unit);
  CAMLlocal4(ret, tbl, list, str);
  ret = caml_alloc(4, 0);

  if (caml_params->section_table != NULL) {
    /* cf. Symtable.bytecode_sections */
    const char* sec_names[] = {"SYMB", "CRCS"};
    tbl = caml_input_value_from_block(caml_params->section_table,
                                      caml_params->section_table_size);
    for (int i = 0; i < sizeof(sec_names)/sizeof(sec_names[0]); i++) {
      for (int j = 0; j < Wosize_val(tbl); j++) {
        value kv = Field(tbl, j);
        if (!strcmp(sec_names[i], String_val(Field(kv, 0))))
          Store_field(ret, i, Field(kv, 1));
      }
    }
  } else {
    struct exec_trailer trail;
    int fd, err;
    char *sect;
    int32_t len;

    fd = open_os(caml_params->exe_name, O_RDONLY | O_BINARY);
    if (fd < 0)
      caml_failwith("Dynlink: Failed to re-open bytecode executable");

    err = caml_read_trailer(fd, &trail);
    if (err != 0)
      caml_failwith("Dynlink: Failed to re-read bytecode trailer");

    caml_read_section_descriptors(fd, &trail);

    len = caml_seek_optional_section(fd, &trail, "SYMB");
    sect = caml_stat_alloc(len);
    if (read(fd, sect, len) != len)
      caml_failwith("Dynlink: error reading SYMB");
    Store_field(ret, 0,
      caml_input_value_from_block(sect, len));
    caml_stat_free(sect);

    len = caml_seek_optional_section(fd, &trail, "CRCS");
    if (len > 0) {
      sect = caml_stat_alloc(len);
      if (read(fd, sect, len) != len)
        caml_failwith("Dynlink: error reading CRCS");
      Store_field(ret, 1,
        caml_input_value_from_block(sect, len));
      caml_stat_free(sect);
    }

    caml_stat_free(trail.section);
    close(fd);
  }

  list = Val_emptylist;
  for (int i = caml_prim_name_table.size - 1; i >= 0; i--) {
    str = caml_copy_string(caml_prim_name_table.contents[i]);
    list = caml_alloc_2(Tag_cons, str, list);
  }
  Store_field(ret, 2, list);

  list = Val_emptylist;
  for (int i = caml_shared_libs_path.size - 1; i >= 0; i--) {
    str = caml_copy_string_of_os(caml_shared_libs_path.contents[i]);
    list = caml_alloc_2(Tag_cons, str, list);
  }
  Store_field(ret, 3, list);

  CAMLreturn (ret);
}

#endif /* NATIVE_CODE */

/** dlopen interface for the bytecode linker **/

#define Handle_val(v) (*((void **) (v)))

CAMLprim value caml_dynlink_open_lib(value filename)
{
  void * handle;
  value result;
  char_os * p;

  CAML_GC_MESSAGE(STARTUP, "Opening shared library %s\n",
                  String_val(filename));
  p = caml_stat_strdup_to_os(String_val(filename));
  caml_enter_blocking_section();
  handle = caml_dlopen(p, 1);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (handle == NULL) caml_failwith(caml_dlerror());
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = handle;
  return result;
}

CAMLprim value caml_dynlink_close_lib(value handle)
{
  caml_dlclose(Handle_val(handle));
  return Val_unit;
}

/*#include <stdio.h>*/
CAMLprim value caml_dynlink_lookup_symbol(value handle, value symbolname)
{
  void * symb;
  value result;
  symb = caml_dlsym(Handle_val(handle), String_val(symbolname));
  /* printf("%s = 0x%lx\n", String_val(symbolname), symb);
     fflush(stdout); */
  if (symb == NULL) return Val_unit /*caml_failwith(caml_dlerror())*/;
  result = caml_alloc_small(1, Abstract_tag);
  Handle_val(result) = symb;
  return result;
}

#ifndef NATIVE_CODE

CAMLprim value caml_dynlink_add_primitive(value handle)
{
  return Val_int(caml_ext_table_add(&caml_prim_table, Handle_val(handle)));
}

CAMLprim value caml_dynlink_get_current_libs(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc_tuple(shared_libs.size);
  for (int i = 0; i < shared_libs.size; i++) {
    value v = caml_alloc_small(1, Abstract_tag);
    Handle_val(v) = shared_libs.contents[i];
    Store_field(res, i, v);
  }
  CAMLreturn(res);
}

#else

value caml_dynlink_add_primitive(value handle)
{
  caml_invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value caml_dynlink_get_current_libs(value unit)
{
  caml_invalid_argument("dynlink_get_current_libs");
  return Val_unit; /* not reached */
}

value caml_dynlink_get_bytecode_sections(value unit)
{
  caml_invalid_argument("dynlink_get_bytecode_sections");
  return Val_unit; /* not reached */
}

#endif /* NATIVE_CODE */
