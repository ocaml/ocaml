/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Interface with C primitives. */

#ifndef CAML_PRIMS_H
#define CAML_PRIMS_H

#ifdef CAML_INTERNALS

typedef value (*c_primitive)();

CAMLdata c_primitive caml_builtin_cprim[];
CAMLdata char * caml_names_of_builtin_cprim[];

CAMLdata struct ext_table caml_prim_table;
#ifdef DEBUG
CAMLdata struct ext_table caml_prim_name_table;
#endif

#define Primitive(n) ((c_primitive)(caml_prim_table.contents[n]))

CAMLdata char * caml_section_table;
CAMLdata asize_t caml_section_table_size;

#endif /* CAML_INTERNALS */

#endif /* CAML_PRIMS_H */
