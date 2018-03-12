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

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((intnat *)((sp) + 92))
#define Callback_link(sp) ((struct caml_context *)((sp) + 104)) //FIXME KC
#endif

#ifdef TARGET_i386
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#ifndef SYS_win32
#define Callback_link(sp) ((struct caml_context *)((sp) + 16)) //FIXME KC
#else
#define Callback_link(sp) ((struct caml_context *)((sp) + 8)) //FIXME KC
#endif
#endif

#ifdef TARGET_power
#if defined(MODEL_ppc)
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#elif defined(MODEL_ppc64)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#define Callback_link(sp) ((struct caml_context *)((sp) + (48 + 32)))
#elif defined(MODEL_ppc64le)
#define Saved_return_address(sp) *((intnat *)((sp) + 16))
#define Callback_link(sp) ((struct caml_context *)((sp) + (32 + 32)))
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
#define Callback_link(sp) ((struct caml_context *)((sp) + Trap_frame_size)) //FIXME KC
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#define Callback_link(sp) ((struct caml_context *)((sp) + 8)) //FIXME KC
#endif

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Context_needs_padding /* keep stack 16-byte aligned */
#endif

/* Structure of OCaml callback contexts */

struct caml_context {
  uintnat exception_ptr_offset; /* exception pointer offset from top of stack */
  value * gc_regs;              /* pointer to register block */
#ifdef Context_needs_padding
  value padding;
#endif
};

/* Declaration of variables used in the asm code */
extern value caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

CAMLextern frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp);

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
