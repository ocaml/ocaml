/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Dynamic loading of C primitives. */

#ifndef CAML_DYNLINK_H
#define CAML_DYNLINK_H

#include "misc.h"

/* Build the table of primitives, given a search path, a list
   of shared libraries, and a list of primitive names
   (all three 0-separated in char arrays).
   Abort the runtime system on error. */
extern void caml_build_primitive_table(char * lib_path,
                                       char * libs,
                                       char * req_prims);

/* The search path for shared libraries */
extern struct ext_table caml_shared_libs_path;

#endif /* CAML_DYNLINK_H */
