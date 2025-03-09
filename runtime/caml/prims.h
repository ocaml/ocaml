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

typedef value (*c_primitive)(void);

extern const c_primitive caml_builtin_cprim[];
extern const char * const caml_names_of_builtin_cprim[];

extern struct ext_table caml_prim_table;
#ifdef DEBUG
extern struct ext_table caml_prim_name_table;
#endif

#define Primitive1(n) \
    ((value (*)(value)) caml_prim_table.contents[n])
#define Primitive2(n) \
    ((value (*)(value,value)) caml_prim_table.contents[n])
#define Primitive3(n) \
    ((value (*)(value,value,value)) caml_prim_table.contents[n])
#define Primitive4(n) \
    ((value (*)(value,value,value,value)) caml_prim_table.contents[n])
#define Primitive5(n) \
    ((value (*)(value,value,value,value,value)) caml_prim_table.contents[n])
#define PrimitiveN(n) \
    ((value (*)(value*,int)) caml_prim_table.contents[n])

extern char * caml_section_table;
extern asize_t caml_section_table_size;

#endif /* CAML_INTERNALS */

#endif /* CAML_PRIMS_H */
