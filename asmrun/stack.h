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

/* Declaration of variables defined in the asm code */
extern char * caml_bottom_of_stack;
extern unsigned long caml_last_return_address;
extern char * caml_exception_pointer;
#define MAX_NUM_GC_REGS 32
extern value gc_entry_regs[MAX_NUM_GC_REGS];
#define MAX_NUM_GC_FLOAT_REGS 32
extern double gc_entry_float_regs[MAX_NUM_GC_FLOAT_REGS];
extern value caml_globals[];
extern long * caml_frametable[];

/* Macros to access the stack frame */
#ifdef TARGET_alpha
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Already_scanned(sp, retaddr) (retaddr & 1L)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 8)) = retaddr | 1L)
#define Mask_already_scanned(retaddr) (retaddr & ~1L)
#define Callback_link(sp) ((struct callback_link *)(sp + 16))
#endif

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((long *)(sp + 92))
#define Callback_link(sp) ((struct callback_link *)(sp + 104))
#endif

#ifdef TARGET_i386
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct callback_link *)(sp + 8))
#endif

#ifdef TARGET_mips
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct callback_link *)(sp + 8))
#endif

#ifdef TARGET_hppa
#define Stack_grows_upwards
#define Saved_return_address(sp) *((long *)sp)
#define Callback_link(sp) ((struct callback_link *)(sp - 16))
#endif

#ifdef TARGET_power
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 4)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
#ifdef SYS_aix
#define Trap_frame_size 24
#else
#define Trap_frame_size 8
#endif
#define Callback_link(sp) ((struct callback_link *)(sp + Trap_frame_size))
#endif

#ifdef TARGET_m68k
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct callback_link *)(sp + 8))
#endif


#endif /* _stack_ */
