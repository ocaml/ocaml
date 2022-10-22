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

/* Macros to access the stack frame */

#ifdef TARGET_i386
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#endif

#ifdef TARGET_power
#if defined(MODEL_ppc)
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#elif defined(MODEL_ppc64)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#elif defined(MODEL_ppc64le)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#else
#error "TARGET_power: wrong MODEL"
#endif
#define Already_scanned(sp, retaddr) ((retaddr) & 1)
#define Mask_already_scanned(retaddr) ((retaddr) & ~1)
#define Mark_scanned(sp, retaddr) Saved_return_address(sp) = (retaddr) | 1
#endif

#ifdef TARGET_s390x
#define Saved_return_address(sp) *((intnat *)((sp) - SIZEOF_PTR))
#define Trap_frame_size 16
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#endif

#ifdef TARGET_amd64
/* Size of the gc_regs structure, in words.
   See amd64.S and amd64/proc.ml for the indices */
#define Wosize_gc_regs (13 /* int regs */ + 16 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#ifdef WITH_FRAME_POINTERS
#define Pop_frame_pointer(sp) (sp) += sizeof(value)
#else
#define Pop_frame_pointer(sp)
#endif
#endif

#ifdef TARGET_arm64
/* Size of the gc_regs structure, in words.
   See arm64.S and arm64/proc.ml for the indices */
#define Wosize_gc_regs (2 + 24 /* int regs */ + 24 /* float regs */)
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Pop_frame_pointer(sp) sp += sizeof(value)
#endif

#ifdef TARGET_riscv
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern intnat caml_globals_inited;

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
