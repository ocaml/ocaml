/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Interface with C primitives. */

#ifndef _prims_
#define _prims_

typedef value (*c_primitive)();

extern c_primitive builtin_cprim[];
extern char * names_of_builtin_cprim[];

extern struct ext_table prim_table;
#ifdef DEBUG
extern struct ext_table prim_name_table;
#endif

#define Primitive(n) ((c_primitive)(prim_table.contents[n]))

#endif /* _prims_ */
