/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Machine-dependent interface with the asm code */

#ifndef _stack_
#define _stack_

/* Macros to access the stack frame */
#ifdef TARGET_alpha
#define NUM_GC_REGS 32
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Already_scanned(sp, retaddr) (retaddr & 1L)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 8)) = retaddr | 1L)
#define Mask_already_scanned(retaddr) (retaddr & ~1L)
#define Callback_link(sp) (*(struct caml_context **)(sp + 16))
#endif

#ifdef TARGET_sparc
#define NUM_GC_REGS 25
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) (*(struct caml_context **)(sp + 8))
#endif

#ifdef TARGET_i386
#define NUM_GC_REGS 7
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) (*(struct caml_context **)(sp + 8))
#endif

#ifdef TARGET_mips
#define NUM_GC_REGS 32
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) (*(struct caml_context **)(sp + 8))
#endif

#ifdef TARGET_hppa
#define NUM_GC_REGS 32
#define Stack_grows_upwards
#define Saved_return_address(sp) *((long *)sp)
#define Callback_link(sp) (*(struct caml_context **)(sp - 16))
#endif

#ifdef TARGET_power
#define NUM_GC_REGS 32
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 4)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
#ifdef SYS_aix
#define Trap_frame_size 24
#else
#define Trap_frame_size 8
#endif
#define Callback_link(sp) (*(struct caml_context **)(sp + Trap_frame_size))
#endif

#ifdef TARGET_m68k
#define NUM_GC_REGS 7
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) (*(struct caml_context **)(sp + 8))
#endif

/* Structure of Caml contexts in the stack */

struct caml_context {
  value gc_regs[NUM_GC_REGS];   /* registers containing heap addresses */
  unsigned long last_retaddr;   /* last return address in Caml code */
  char stack_chunk[sizeof(value)]; /* beginning of the stack chunk */
};

/* Structure of a Caml stack chunk is:
      - pointer to context for previous chunk
      - frame with return address into callback (negative frame size)
      - regular Caml frames
      - context for this chunk
*/

/* Declaration of variables defined in the asm code */
extern struct caml_context * caml_last_context;
extern char * caml_exception_pointer;
extern value caml_globals[];
extern long * caml_frametable[];


#endif /* _stack_ */
