/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Machine-dependent macros to access the stack frames. */

#ifndef _stack_
#define _stack_


#ifdef TARGET_alpha
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 8)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
#define Callback_link(sp) ((struct callback_link *)sp)
#endif

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((long *)(sp + 92))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp + 92)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
#define Callback_link(sp) ((struct callback_link *)(sp + 96))
#endif

#ifdef TARGET_i386
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct callback_link *)sp)
#endif

#ifdef TARGET_mips
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 4)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
#define Callback_link(sp) ((struct callback_link *)sp)
#endif

#ifdef TARGET_hppa
#define Stack_grows_upwards
#define Saved_return_address(sp) *((long *)sp)
#define Already_scanned(sp, retaddr) (retaddr & 0x80000000)
#define Mark_scanned(sp, retaddr) (*((long *)sp) = retaddr | 0x80000000)
#define Mask_already_scanned(retaddr) (retaddr & ~0x80000000)
#define Callback_link(sp) ((struct callback_link *)(sp - 8))
#endif


#endif /* _stack_ */
