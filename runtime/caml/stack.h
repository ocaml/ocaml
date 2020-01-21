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
#ifndef SYS_win32
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#else
#define Callback_link(sp) ((struct caml_context *)((sp) + 8))
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
#define Callback_link(sp) ((struct caml_context *)((sp) + Trap_frame_size))
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#define Callback_link(sp) ((struct caml_context *)((sp) + 8))
#endif

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#endif

/* Structure of OCaml callback contexts */

struct caml_context {
  char * bottom_of_stack;       /* beginning of OCaml stack chunk */
  uintnat last_retaddr;         /* last return address in OCaml code */
  value * gc_regs;              /* pointer to register block */
#ifdef WITH_SPACETIME
  void* trie_node;
#endif
};

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1 /* num_live */];
  /*
    If frame_size & 2, then allocation info follows:
  unsigned char num_allocs;
  unsigned char alloc_lengths[num_alloc];

    If frame_size & 1, then debug info follows:
  uint32_t debug_info_offset[num_debug];

    Debug info is stored as relative offsets to debuginfo structures.
    num_debug is num_alloc if frame_size & 2, otherwise 1. */
} frame_descr;

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & -sizeof(ty))


/* Hash table of frame descriptors */

extern frame_descr ** caml_frame_descriptors;
extern uintnat caml_frame_descriptors_mask;

#define Hash_retaddr(addr) \
  (((uintnat)(addr) >> 3) & caml_frame_descriptors_mask)

extern void caml_init_frame_descriptors(void);
extern void caml_register_frametable(intnat *);
extern void caml_unregister_frametable(intnat *);
extern void caml_register_dyn_global(void *);

extern uintnat caml_stack_usage (void);
extern uintnat (*caml_stack_usage_hook)(void);

/* Declaration of variables used in the asm code */
extern value * caml_globals[];
extern char caml_globals_map[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

/* Global variables moved to Caml_state in 4.10 */
#define caml_top_of_stack (Caml_state_field(top_of_stack))
#define caml_bottom_of_stack (Caml_state_field(bottom_of_stack))
#define caml_last_return_address (Caml_state_field(last_return_address))
#define caml_gc_regs (Caml_state_field(gc_regs))
#define caml_exception_pointer (Caml_state_field(exception_pointer))

CAMLextern frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp);

#endif /* CAML_INTERNALS */

#endif /* CAML_STACK_H */
