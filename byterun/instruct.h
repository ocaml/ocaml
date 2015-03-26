/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* The instruction set. */

#ifndef CAML_INSTRUCT_H
#define CAML_INSTRUCT_H

enum instructions {
#define Instruction(name) name,
#include "instruct.tbl"
#undef Instruction
FIRST_UNIMPLEMENTED_OP};


#endif /* CAML_INSTRUCT_H */
