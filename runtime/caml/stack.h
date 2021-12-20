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
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Context_needs_padding /* keep stack 16-byte aligned */
#endif

#ifdef TARGET_riscv
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

/* Structure of OCaml callback contexts */

struct caml_context {
  uintnat exception_ptr;        /* exception pointer */
  value * gc_regs;              /* pointer to register block */
#ifdef Context_needs_padding
  value padding;
#endif
};

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern intnat caml_globals_inited;

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
