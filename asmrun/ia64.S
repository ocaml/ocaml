/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, Alpha processor */

#define BROKEN_POSTINCREMENT

#define ADDRGLOBAL(reg,symb) \
  add reg = @ltoff(symb), gp; ld8 reg = [reg]
#define LOADGLOBAL(reg,symb) \
  add r3 = @ltoff(symb), gp; ld8 r3 = [r3]; ld8 reg = [r3]
#define STOREGLOBAL(reg,symb) \
  add r3 = @ltoff(symb), gp; ld8 r3 = [r3]; st8 [r3] = reg

#ifdef BROKEN_POSTINCREMENT
#define ST8OFF(a,b,d) st8 [a] = b; add a = d, a
#define LD8OFF(a,b,d) ld8 a = [b]; add b = d, b
#define STFDOFF(a,b,d) stfd [a] = b; add a = d, a
#define LDFDOFF(a,b,d) ldfd a = [b]; add b = d, b
#define STFSPILLOFF(a,b,d) stf.spill [a] = b; add a = d, a
#define LDFFILLOFF(a,b,d) ldf.fill a = [b]; add b = d, b
#else
#define ST8OFF(a,b,d) st8 [a] = b, d
#define LD8OFF(a,b,d) ld8 a = [b], d
#define STFDOFF(a,b,d) stfd [a] = b, d
#define LDFDOFF(a,b,d) ldfd a = [b], d
#define STFSPILLOFF(a,b,d) stf.spill [a] = b, d
#define LDFFILLOFF(a,b,d) ldf.fill a = [b], d
#endif

#define SAVE2(a,b) ST8OFF(r2, a, 16); ST8OFF(r3, b, 16)
#define SAVE4(a,b,c,d) SAVE2(a,b); SAVE2(c,d)
#define SAVE8(a,b,c,d,e,f,g,h) SAVE4(a,b,c,d); SAVE4(e,f,g,h)

#define LOAD2(a,b) LD8OFF(a, r2, 16); LD8OFF(b, r3, 16)
#define LOAD4(a,b,c,d) LOAD2(a,b); LOAD2(c,d)
#define LOAD8(a,b,c,d,e,f,g,h) LOAD4(a,b,c,d); LOAD4(e,f,g,h)

#define FSAVE2(a,b) STFDOFF(r2, a, 16); STFDOFF(r3, b, 16)
#define FSAVE4(a,b,c,d) FSAVE2(a,b); FSAVE2(c,d)
#define FSAVE8(a,b,c,d,e,f,g,h) FSAVE4(a,b,c,d); FSAVE4(e,f,g,h)

#define FLOAD2(a,b) LDFDOFF(a, r2, 16); LDFDOFF(b, r3, 16)
#define FLOAD4(a,b,c,d) FLOAD2(a,b); FLOAD2(c,d)
#define FLOAD8(a,b,c,d,e,f,g,h) FLOAD4(a,b,c,d); FLOAD4(e,f,g,h)

#define FSPILL2(a,b) STFSPILLOFF(r2, a, 32); STFSPILLOFF(r3, b, 32)
#define FSPILL4(a,b,c,d) FSPILL2(a,b); FSPILL2(c,d)
#define FSPILL8(a,b,c,d,e,f,g,h) FSPILL4(a,b,c,d); FSPILL4(e,f,g,h)

#define FFILL2(a,b) LDFFILLOFF(a, r2, 32); LDFFILLOFF(b, r3, 32)
#define FFILL4(a,b,c,d) FFILL2(a,b); FFILL2(c,d)
#define FFILL8(a,b,c,d,e,f,g,h) FFILL4(a,b,c,d); FFILL4(e,f,g,h)

/* Allocation */
	.text

        .global caml_alloc#
        .proc   caml_alloc#
        .align 16

/* caml_alloc: all code generator registers preserved,
   gp preserved, r2 = requested size */

caml_alloc:
        sub     r4 = r4, r2
        cmp.ltu p0, p6 = r4, r5
        (p6) br.ret.sptk b0
        /* Stash return address at sp (in stack scratch area) */
        mov     r3 = b0
        st8     [sp] = r3
        /* Call GC */
        br.call.sptk b0 = caml_call_gc#
        /* Return to caller */
        ld8     r3 = [sp]
        mov     b0 = r3
        br.ret.sptk b0

        .endp   caml_alloc#

/* caml_call_gc: all code generator registers preserved,
   gp preserved, r2 = requested size */

        .global caml_call_gc#
        .proc   caml_call_gc#
        .align 16
caml_call_gc:
        /* Allocate stack frame */
        add     sp = -(16 + 16 + 80*8 + 42*8), sp

        /* Save requested size and GP on stack */
        add     r3 = 16, sp
        ST8OFF(r3, r2, 8)
        st8     [r3] = gp

        /* Record lowest stack address, return address, GC regs */
        mov     r2 = b0
        STOREGLOBAL(r2, caml_last_return_address#)
        add     r2 = (16 + 16 + 80*8 + 42*8), sp
        STOREGLOBAL(r2, caml_bottom_of_stack#)
        add     r2 = (16 + 16), sp
        STOREGLOBAL(r2, caml_gc_regs#)

        /* Save all integer regs used by the code generator in the context */
.L100:  add     r3 = 8, r2
        SAVE4(r8,r9,r10,r11)
        SAVE8(r16,r17,r18,r19,r20,r21,r22,r23)
        SAVE8(r24,r25,r26,r27,r28,r29,r30,r31)
        SAVE8(r32,r33,r34,r35,r36,r37,r38,r39)
        SAVE8(r40,r41,r42,r43,r44,r45,r46,r47)
        SAVE8(r48,r49,r50,r51,r52,r53,r54,r55)
        SAVE8(r56,r57,r58,r59,r60,r61,r62,r63)
        SAVE8(r64,r65,r66,r67,r68,r69,r70,r71)
        SAVE8(r72,r73,r74,r75,r76,r77,r78,r79)
        SAVE8(r80,r81,r82,r83,r84,r85,r86,r87)
        SAVE4(r88,r89,r90,r91)

        /* Save all floating-point registers not preserved by C */
        FSAVE2(f6,f7)
        FSAVE8(f8,f9,f10,f11,f12,f13,f14,f15)
        FSAVE8(f32,f33,f34,f35,f36,f37,f38,f39)
        FSAVE8(f40,f41,f42,f43,f44,f45,f46,f47)
        FSAVE8(f48,f49,f50,f51,f52,f53,f54,f55)
        FSAVE8(f56,f57,f58,f59,f60,f61,f62,f63)

        /* Save current allocation pointer for debugging purposes */
        STOREGLOBAL(r4, young_ptr#)

        /* Save trap pointer in case an exception is raised */
        STOREGLOBAL(r6, caml_exception_pointer#)

        /* Call the garbage collector */
        br.call.sptk    b0 = garbage_collection#

        /* Restore gp */
        add     r3 = 24, sp
        ld8     gp = [r3]

        /* Restore all integer regs from GC context */
        add     r2 = (16 + 16), sp
        add     r3 = 8, r2
        LOAD4(r8,r9,r10,r11)
        LOAD8(r16,r17,r18,r19,r20,r21,r22,r23)
        LOAD8(r24,r25,r26,r27,r28,r29,r30,r31)
        LOAD8(r32,r33,r34,r35,r36,r37,r38,r39)
        LOAD8(r40,r41,r42,r43,r44,r45,r46,r47)
        LOAD8(r48,r49,r50,r51,r52,r53,r54,r55)
        LOAD8(r56,r57,r58,r59,r60,r61,r62,r63)
        LOAD8(r64,r65,r66,r67,r68,r69,r70,r71)
        LOAD8(r72,r73,r74,r75,r76,r77,r78,r79)
        LOAD8(r80,r81,r82,r83,r84,r85,r86,r87)
        LOAD4(r88,r89,r90,r91)

        /* Restore all floating-point registers not preserved by C */
        FLOAD2(f6,f7)
        FLOAD8(f8,f9,f10,f11,f12,f13,f14,f15)
        FLOAD8(f32,f33,f34,f35,f36,f37,f38,f39)
        FLOAD8(f40,f41,f42,f43,f44,f45,f46,f47)
        FLOAD8(f48,f49,f50,f51,f52,f53,f54,f55)
        FLOAD8(f56,f57,f58,f59,f60,f61,f62,f63)

        /* Reload new allocation pointer and allocation limit */
        LOADGLOBAL(r4, young_ptr#)
        LOADGLOBAL(r5, young_limit#)

        /* Allocate space for the block */
        add     r3 = 16, sp
        ld8     r2 = [r3]
        sub     r4 = r4, r2
        cmp.ltu p6, p0 = r4, r5         /* enough space? */
        (p6) br.cond.spnt .L100          /* no: call GC again */

        /* Reload return address and say that we are back into Caml code */
        ADDRGLOBAL(r3, caml_last_return_address#)
        ld8     r2 = [r3]
        st8     [r3] = r0

        /* Return to caller */
        mov     b0 = r2
        add     sp = (16 + 16 + 80*8 + 42*8), sp
        br.ret.sptk b0

        .endp   caml_call_gc#

/* Call a C function from Caml */
/* Function to call is in r2 */

        .global caml_c_call#
        .proc   caml_c_call#
        .align  16

caml_c_call:
        /* The Caml code that called us does not expect any
           code-generator registers to be preserved */

        /* Recover entry point from the function pointer in r2 */
        LD8OFF(r3, r2, 8)
        mov     b6 = r3

        /* Preserve gp in r7 */
        mov     r7 = gp

        /* Record lowest stack address and return address */
	mov	r14 = b0
        STOREGLOBAL(sp, caml_bottom_of_stack#)
        STOREGLOBAL(r14, caml_last_return_address#)

        /* Make the exception handler and alloc ptr available to the C code */
        STOREGLOBAL(r4, young_ptr#)
        STOREGLOBAL(r6, caml_exception_pointer#)

        /* Recover gp from the function pointer in r2 */
        ld8     gp = [r2]

        /* Call the function */
        br.call.sptk    b0 = b6

        /* Restore gp */
        mov     gp = r7

        /* Reload alloc ptr and alloc limit */
        /* gas bug LOADGLOBAL(r4, young_ptr#) */
        add r3 = @ltoff(young_ptr#), r7; ld8 r3 = [r3]; ld8 r4 = [r3]
        LOADGLOBAL(r5, young_limit#)

        /* Reload return address and say that we are back into Caml code */
        ADDRGLOBAL(r3, caml_last_return_address#)
        ld8     r2 = [r3]
        st8     [r3] = r0

        /* Return to caller */
        mov     b0 = r2
        br.ret.sptk b0

        .endp   caml_c_call#

/* Start the Caml program */

        .global caml_start_program#
        .proc   caml_start_program#
        .align  16

caml_start_program:
        ADDRGLOBAL(r2, caml_program#)
        mov     b6 = r2

        /* Code shared with callback* */
.L103:        
        /* Allocate 64 "out" registers (for the Caml code) and no locals */
        alloc	r3 = ar.pfs, 0, 0, 64, 0

        /* Save PFS, return address and GP on stack */
        add     sp = -(20 * 16 + 4 * 8 + 4 * 8), sp
        add     r2 = 16, sp
	ST8OFF(r2, r3, 8)
        mov     r3 = b0
        ST8OFF(r2, r3, 8)
        ST8OFF(r2, gp, 8)

        /* Save predicates on stack */
        mov     r3 = pr
        ST8OFF(r2, r3, 8)

        /* Save callee-save integer registers on stack */
        add     r3 = 8, r2
        SAVE4(r4,r5,r6,r7)

        /* Save callee-save floating-point registers on stack */
        add     r3 = 16, r2
        FSPILL4(f2,f3,f4,f5)
        FSPILL8(f16,f17,f18,f19,f20,f21,f22,f23)
        FSPILL8(f24,f25,f26,f27,f28,f29,f30,f31)

        /* Set up a callback link on the stack */
        add     sp = -32, sp
        LOADGLOBAL(r3, caml_bottom_of_stack#)
        add     r2 = 16, sp
        ST8OFF(r2, r3, 8)
        LOADGLOBAL(r3, caml_last_return_address#)
        ST8OFF(r2, r3, 8)
        LOADGLOBAL(r3, caml_gc_regs#)
        ST8OFF(r2, r3, 8)

        /* Set up a trap frame to catch exceptions escaping the Caml code */
        mov     r6 = sp
        add     sp = -16, sp
        LOADGLOBAL(r3, caml_exception_pointer#)
        add     r2 = 16, sp
        ST8OFF(r2, r3, 8)
        LOADGLOBAL(r3, .L101)
        ST8OFF(r2, r3, 8)

        /* Reload allocation pointers */
        LOADGLOBAL(r4, young_ptr#)
        LOADGLOBAL(r5, young_limit#)

        /* We are back into Caml code */
        STOREGLOBAL(r0, caml_last_return_address#)

        /* Call the Caml code */
        br.call.sptk b0 = b6
.L102:

        /* Pop the trap frame, restoring caml_exception_pointer */
        add     sp = 16, sp
        ld8     r2 = [sp]
        STOREGLOBAL(r2, caml_exception_pointer#)

.L104:
        /* Pop the callback link, restoring the global variables */
        add     r14 = 16, sp
        LD8OFF(r2, r14, 8)
        STOREGLOBAL(r2, caml_bottom_of_stack#)
        LD8OFF(r2, r14, 8)
        STOREGLOBAL(r2, caml_last_return_address#)
        LD8OFF(r2, r14, 8)
        STOREGLOBAL(r2, caml_gc_regs#)
        add     sp = 32, sp

        /* Update allocation pointer */
        STOREGLOBAL(r4, young_ptr#)

        /* Restore PFS, return address and GP from stack */
        add     r2 = 16, sp
        LD8OFF(r3, r2, 8)
        mov     ar.pfs = r3
        LD8OFF(r3, r2, 8)
        mov     b0 = r3
        LD8OFF(gp, r2, 8)

        /* Restore the predicates */
        LD8OFF(r3, r2, 8)
        mov     pr = r3, -1

        /* Restore the saved integer register */
        add     r3 = 8, r2
        LOAD4(r4,r5,r6,r7)

        /* Restore the saved floating-point registers */
        add     r3 = 16, r2
        FFILL4(f2,f3,f4,f5)
        FFILL8(f16,f17,f18,f19,f20,f21,f22,f23)
        FFILL8(f24,f25,f26,f27,f28,f29,f30,f31)

        /* Pop stack frame and return */
        add     sp = (20 * 16 + 4 * 8 + 4 * 8), sp
        br.ret.sptk.many b0

        /* The trap handler */
.L101:
        /* Save exception pointer */
        STOREGLOBAL(r6, caml_exception_pointer#)

        /* Encode exception bucket as exception result */
        or      r8 = 2, r8

        /* Return it */
        br.sptk .L104

        .endp   caml_start_program#

/* Raise an exception from C */

        .global raise_caml_exception#
        .proc   raise_caml_exception#
        .align  16
raise_caml_exception:
        /* Allocate 64 "out" registers (for the Caml code) and no locals */
        /* Since we don't return, don't bother saving the PFS */
        alloc	r2 = ar.pfs, 0, 0, 64, 0

        /* Move exn bucket where Caml expects it */
        mov     r8 = r32

        /* Reload allocation pointers and exception pointer */
        LOADGLOBAL(r4, young_ptr#)
        LOADGLOBAL(r5, young_limit#)
        LOADGLOBAL(r6, caml_exception_pointer#)

        /* Say that we're back into Caml */
        STOREGLOBAL(r0, caml_last_return_address#)

        /* Raise the exception proper */
        mov     sp = r6
        add     r2 = 8, r6
        ld8     r6 = [r6]
        ld8     r2 = [r2]
        mov     b6 = r2

	/* Branch to handler.  Must use a call so as to set up the
	   CFM and PFS correctly. */
        br.call.sptk.many b0 = b6

        .endp   raise_caml_exception

/* Callbacks from C to Caml */

        .global callback_exn#
        .proc   callback_exn#
        .align  16
callback_exn:
        /* Initial shuffling of arguments */
        ld8     r3 = [r32]              /* code pointer */
        mov     r2 = r32
        mov     r32 = r33               /* first arg */
        mov     r33 = r2                /* environment */
        mov     b6 = r3
        br.sptk .L103

        .endp   callback_exn#

        .global callback2_exn#
        .proc   callback2_exn#
        .align  16
callback2_exn:
        /* Initial shuffling of arguments */
        ADDRGLOBAL(r3, caml_apply2)    /* code pointer */
        mov     r2 = r32
        mov     r32 = r33               /* first arg */
        mov     r33 = r34               /* second arg */
        mov     r34 = r2                /* environment */
        mov     b6 = r3
        br.sptk .L103

        .endp   callback2_exn#

        .global callback3_exn#
        .proc   callback3_exn#
        .align  16
callback3_exn:
        /* Initial shuffling of arguments */
        ADDRGLOBAL(r3, caml_apply3)    /* code pointer */
        mov     r2 = r32
        mov     r32 = r33               /* first arg */
        mov     r33 = r34               /* second arg */
        mov     r34 = r35               /* third arg */
        mov     r35 = r2                /* environment */
        mov     b6 = r3
        br.sptk .L103

        .endp   callback3_exn#

/* Glue code to call array_bound_error */

        .global caml_array_bound_error#
        .proc   caml_array_bound_error#
        .align  16
caml_array_bound_error:
        ADDRGLOBAL(r2, @fptr(array_bound_error#))
        br.sptk caml_c_call             /* never returns */        

        .rodata

        .global system_frametable#
        .type   system_frametable#, @object
        .size   system_frametable#, 8
system_frametable:
        data8   1               /* one descriptor */
        data8   .L102           /* return address into callback */
        data2   -1              /* negative frame size => use callback link */
        data2   0               /* no roots here */
        .align  8
