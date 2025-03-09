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

/* Machine-dependent interface with the asm code */

#ifndef CAML_STACK_H
#define CAML_STACK_H

#ifdef CAML_INTERNALS

/* Macros to access OCaml stacks */

/* An OCaml stack is composed of one or several "chunks", each chunk
   being a sequence of frames (activation records) for ocamlopt-generated
   functions.

   A chunk terminates when the OCaml code calls into C code
   (explicitly or to perform garbage collection or signal polling).

   A chunk starts when the program starts, or a fiber is created,
   or a callback is performed from C to OCaml.

   If [sp] points to the bottom of an OCaml stack,
   [First_frame(sp)] is the first stack frame of the first chunk of this stack.

   If [sp] points to the special frame for [caml_start_program] or
   [caml_callback_*], this marks the end of the current chunk.
   The saved value of [gc_regs] for the previous chunk is in
   [Saved_gc_regs(sp)], and [Stack_header_size] bytes must be skipped
   to find the first frame of the next chunk, or to reach the top of the stack.
*/

#ifdef TARGET_power
/* Size of the gc_regs structure, in words.
   See power.S and power/proc.ml for the indices */
#define Wosize_gc_regs (23 /* int regs */ + 14 /* caller-save float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#define First_frame(sp) (sp)
#define Saved_gc_regs(sp) (*(value **)((sp) + 32 + 16 + 8))
#define Stack_header_size (32 + 16 + 16)
#endif

#ifdef TARGET_s390x
#define Wosize_gc_regs (2 + 9 /* int regs */ + 16 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 8)
#define Saved_gc_regs(sp) (*(value **)((sp) + 24))
#define Stack_header_size 32
#endif

#ifdef TARGET_amd64
/* Size of the gc_regs structure, in words.
   See amd64.S and amd64/proc.ml for the indices */
#define Wosize_gc_regs (13 /* int regs */ + 16 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#ifdef WITH_FRAME_POINTERS
#define First_frame(sp) ((sp) + 16)
#else
#define First_frame(sp) ((sp) + 8)
#endif
#define Saved_gc_regs(sp) (*(value **)((sp) + 24))
#define Stack_header_size 32
#endif

#ifdef TARGET_arm64
/* Size of the gc_regs structure, in words.
   See arm64.S and arm64/proc.ml for the indices */
#define Wosize_gc_regs (2 + 24 /* int regs */ + 24 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 16)
#define Saved_gc_regs(sp) (*(value **)((sp) + 24))
#define Stack_header_size 32
#endif

#ifdef TARGET_riscv
/* Size of the gc_regs structure, in words.
   See riscv.S and riscv/proc.ml for the indices */
#define Wosize_gc_regs (2 + 22 /* int regs */ + 20 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define First_frame(sp) ((sp) + 16)
#define Saved_gc_regs(sp) (*(value **)((sp) + 24))
#define Stack_header_size 32
#endif

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern intnat caml_globals_inited;

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
