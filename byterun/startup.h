/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef CAML_STARTUP_H
#define CAML_STARTUP_H

#include "misc.h"
#include "exec.h"

CAMLextern void caml_main(char **argv);
CAMLextern void caml_startup_code(code_t code, asize_t code_size,
                                  char *data, char **argv);

enum { FILE_NOT_FOUND = -1, BAD_BYTECODE  = -2 };

extern int attempt_open(char **name, struct exec_trailer *trail,
                        int do_open_script);
extern void read_section_descriptors(int fd, struct exec_trailer *trail);
extern int32 seek_optional_section(int fd, struct exec_trailer *trail,
                                   char *name);
extern int32 seek_section(int fd, struct exec_trailer *trail, char *name);


#endif /* CAML_STARTUP_H */
