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

typedef void * c_primitive;
typedef value (*c_primitive1)(value);
typedef value (*c_primitive2)(value, value);
typedef value (*c_primitive3)(value, value, value);
typedef value (*c_primitive4)(value, value, value, value);
typedef value (*c_primitive5)(value, value, value, value, value);
typedef value (*c_primitiveN)(value *, int);

extern c_primitive caml_builtin_cprim[];
extern char * caml_names_of_builtin_cprim[];

extern struct ext_table caml_prim_table;
#ifdef DEBUG
extern struct ext_table caml_prim_name_table;
#endif

#define Primitive1(n) ((c_primitive1)caml_prim_table.contents[n])
#define Primitive2(n) ((c_primitive2)caml_prim_table.contents[n])
#define Primitive3(n) ((c_primitive3)caml_prim_table.contents[n])
#define Primitive4(n) ((c_primitive4)caml_prim_table.contents[n])
#define Primitive5(n) ((c_primitive5)caml_prim_table.contents[n])
#define PrimitiveN(n) ((c_primitiveN)caml_prim_table.contents[n])

extern char * caml_section_table;
extern asize_t caml_section_table_size;

#endif /* CAML_INTERNALS */

#endif /* CAML_PRIMS_H */
