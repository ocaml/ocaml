/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Operating system - specific stuff */

#ifndef CAML_OSDEPS_H
#define CAML_OSDEPS_H

#include "misc.h"

/* Decompose the given path into a list of directories, and add them
   to the given table.  Return the block to be freed later. */
extern char * caml_decompose_path(struct ext_table * tbl, char * path);

/* Search the given file in the given list of directories.
   If not found, return a copy of [name].  Result is allocated with
   [caml_stat_alloc]. */
extern char * caml_search_in_path(struct ext_table * path, char * name);

/* Same, but search an executable name in the system path for executables. */
CAMLextern char * caml_search_exe_in_path(char * name);

/* Same, but search a shared library in the given path. */
extern char * caml_search_dll_in_path(struct ext_table * path, char * name);

/* Open a shared library and return a handle on it.
   Return [NULL] on error. */
extern void * caml_dlopen(char * libname);

/* Close a shared library handle */
extern void caml_dlclose(void * handle);

/* Look up the given symbol in the given shared library.
   Return [NULL] if not found, or symbol value if found. */
extern void * caml_dlsym(void * handle, char * name);

/* Return an error message describing the most recent dynlink failure. */
extern char * caml_dlerror(void);

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */
extern int caml_read_directory(char * dirname, struct ext_table * contents);

#ifdef __linux__
/* Recover executable name from /proc/self/exe if possible */
extern int caml_executable_name(char * name, int name_len);
#endif

#endif /* CAML_OSDEPS_H */

