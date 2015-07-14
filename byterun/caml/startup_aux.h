/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 Damien Doligez, Jane Street Group, LLC              */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include "config.h"

extern void caml_init_atom_table (void);

extern uintnat caml_init_percent_free;
extern uintnat caml_init_max_percent_free;
extern uintnat caml_init_minor_heap_wsz;
extern uintnat caml_init_heap_chunk_sz;
extern uintnat caml_init_heap_wsz;
extern uintnat caml_init_max_stack_wsz;
extern uintnat caml_trace_level;

extern void caml_parse_ocamlrunparam (void);
