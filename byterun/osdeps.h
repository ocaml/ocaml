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

#ifndef _osdeps_

#define _osdeps_

#include "misc.h"

/* Decompose the given path into a list of directories, and add them
   to the given table.  Return the block to be freed later. */
extern char * decompose_path(struct ext_table * tbl, char * path);

/* Search the given file in the given list of directories.
   If not found, return a copy of [name].  Result is allocated with
   [stat_alloc]. */
extern char * search_in_path(struct ext_table * path, char * name);

/* Same, but search an executable name in the system path for executables. */
CAMLextern char * search_exe_in_path(char * name);

/* Same, but search a shared library in the given path. */
extern char * search_dll_in_path(struct ext_table * path, char * name);

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

#endif

