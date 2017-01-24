/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#include "../config/m.h"

#if defined(SYS_macosx)

#define LBL(x) L##x
#define G(r) _##r
#define GREL(r) _##r@GOTPCREL
#define GCALL(r) _##r
#define FUNCTION_ALIGN 2
#define EIGHT_ALIGN 3
#define SIXTEEN_ALIGN 4
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#elif defined(SYS_mingw64) || defined(SYS_cygwin)

#define LBL(x) .L##x
#define G(r) r
#undef  GREL
#define GCALL(r) r
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#endif

#ifdef WITH_FRAME_POINTERS

#define ENTER_FUNCTION \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp
#define LEAVE_FUNCTION \
        popq    %rbp; CFI_ADJUST(-8);

#else

#define ENTER_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8);
#define LEAVE_FUNCTION \
        addq    $8, %rsp; CFI_ADJUST (-8);

#endif

/******************************************************************************/
/* Access to the current domain state block. */
/******************************************************************************/

#define CAML_CONFIG_H_NO_TYPEDEFS
#include "../byterun/caml/config.h"

#define GET_DOMAIN_STATE(reg) \
	movq	%r15, reg ; \
	andq  $(0xffffffffffffffff << Minor_heap_align_bits), reg

/* CR mshinwell: We should optimize the case where there are multiple
   loads/stores in one go to the domain state block. */

#define DOMAIN_STATE(field_num, c_type, name)            \
  .macro load_caml_##name dest_reg:req ;                 \
	GET_DOMAIN_STATE(\dest_reg) ;                    \
	movq    (8 * field_num)(\dest_reg), \dest_reg ;  \
  .endm ; \
  .macro store_caml_##name src_reg:req ;           \
	GET_DOMAIN_STATE(%r11) ;                   \
	movq    \src_reg, (8 * field_num)(%r11) ;  \
  .endm ; \
  .macro test_caml_##name value:req ;            \
	GET_DOMAIN_STATE(%r11) ;                 \
	testq   \value, (8 * field_num)(%r11) ;  \
  .endm ; \
  .macro cmp_caml_##name reg:req ;               \
	GET_DOMAIN_STATE(%rax) ;                 \
	cmpq	(8 * field_num)(%rax), \reg ;    \
  .endm ; \
  .macro push_caml_##name ;                      \
	load_caml_##name %r11 ;                  \
        pushq   %r11 ;                           \
  .endm ; \
  .macro pop_caml_##name ;                       \
        popq    %r13 ;                           \
        store_caml_##name %r13 ;                 \
  .endm
#include "../byterun/caml/domain_state.tbl"
#undef DOMAIN_STATE

/* Load from the current domain state block.  Clobbers only the destination. */
#define LOAD_TL_VAR(var,dstreg) \
        load_##var dstreg

/* Store to the current domain state block.  Clobbers %r11. */
#define STORE_TL_VAR(srcreg, var) \
        store_##var srcreg

/* Test against a value in the current domain block.  Clobbers %r11. */
#define TEST_TL_VAR(value, var) \
        test_##var value

/* Compare against a value in the current domain block.  Clobbers %rax. */
#define CMP_TL_VAR(var, reg) \
        cmp_##var reg

/* Push a value from the current domain block onto the stack.  Clobbers %r11. */
#define PUSH_TL_VAR(var) \
        push_##var

/* Pop a value from the current domain block onto the stack.
   Clobbers %r11 and %r13. */
#define POP_TL_VAR(var) \
        pop_##var

/* Load address of global [label] in register [dst]. */
#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst
#else
#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst
#endif


/******************************************************************************/
/* Stack switching operations */
/******************************************************************************/

#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)

/* Position-independent operations on global variables. */

/* Load address of global [label] in register [dst]. */
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst

/* Switch from OCaml to C stack. Clobbers REG & %r14. */
#define SWITCH_OCAML_TO_C_NO_CTXT(REG) \
    /* Save OCaml SP in the stack slot */ \
        LOAD_TL_VAR(caml_stack_high, %r14); \
        subq    %r14, %rsp; \
        sarq    $3, %rsp; \
        LOAD_TL_VAR(caml_current_stack, REG); \
        movq    %rsp, (REG); \
    /* Switch to system stack */ \
        LOAD_TL_VAR(caml_system_sp, %rsp)

/* Switch from OCaml to C stack. Also builds a context at
 * the bottom of the OCaml stack. Clobbers REG & %r14. */
#define SWITCH_OCAML_TO_C(REG) \
    /* Build caml_context at the bottom of the stack */ \
        pushq   $0 ; CFI_ADJUST(8); \
        pushq   %r14; CFI_ADJUST(8); \
        SWITCH_OCAML_TO_C_NO_CTXT(REG)

/* Switch from C to OCaml stack.  Clobbers REG. */
#define SWITCH_C_TO_OCAML_NO_CTXT(REG) \
    /* Switch to OCaml stack */ \
        LOAD_TL_VAR(caml_stack_high, %rsp); \
        LOAD_TL_VAR(caml_current_stack, REG); \
    /* REG is Stack_sp(caml_current_stack) */ \
        movq    (REG), REG; \
        salq    $3, REG; \
        addq    REG, %rsp
#else

/* Non-PIC operations on global variables.  Slightly faster. */

#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst

/* Switch from OCaml to C stack. Clobbers REG & %r14. */
#define SWITCH_OCAML_TO_C_NO_CTXT(REG) \
    /* Save OCaml SP in the stack slot */ \
        LOAD_TL_VAR(caml_stack_high, %r14); \
        subq    %r14, %rsp; \
        sarq    $3, %rsp; \
        LOAD_TL_VAR(caml_current_stack, REG); \
        movq    %rsp, (REG); \
    /* Switch to system stack */ \
        LOAD_TL_VAR(caml_system_sp, %rsp)

/* Switch from OCaml to C stack. Also builds a context at
 * the bottom of the OCaml stack. Clobbers REG & %r14. */
#define SWITCH_OCAML_TO_C(REG) \
    /* Build caml_context at the bottom of the stack */ \
        pushq   $0 ; CFI_ADJUST(8); \
        pushq   %r14; CFI_ADJUST(8); \
        SWITCH_OCAML_TO_C_NO_CTXT(REG)

/* Switch from C to OCaml stack. Clobbers REG. */
#define SWITCH_C_TO_OCAML_NO_CTXT(REG) \
    /* Switch to OCaml stack */ \
        LOAD_TL_VAR(caml_stack_high, %rsp); \
        LOAD_TL_VAR(caml_current_stack, REG); \
        movq    (REG), REG; \
        salq    $3, REG; \
        addq    REG, %rsp

#endif

/* Switch from C to OCaml stack. Also pops the context
 * from the bottom of the OCaml stack. Clobbers REG. */
#define SWITCH_C_TO_OCAML(REG) \
        SWITCH_C_TO_OCAML_NO_CTXT(REG); \
    /* Pop the caml_context from the bottom of stack updating %r14 */ \
        popq    %r14; CFI_ADJUST(-8); \
        popq    REG; CFI_ADJUST(-8)

/* Load [caml_stack_high - %r14] into %rsp. %r14 is an offset. Clobbers %r11. */
#define RESTORE_EXN_HANDLER_OCAML \
        LOAD_TL_VAR(caml_stack_high, %rsp); \
        sub     %r14, %rsp; \
        popq    %r14

/* Load [caml_stack_high - %r14] into %rsp. %r14 is an offset. Clobbers %r11. */
#define RESTORE_EXN_HANDLER_SYS \
        LOAD_TL_VAR(caml_system_stack_high, %rsp); \
        sub     %r14, %rsp; \
        popq    %r14

/* Switch between OCaml stacks.
 * arguments : target stack (%rdi)
 * preserves : %rbx, %r12, %r13 */
#define SWITCH_OCAML_STACKS \
    /* Switch from OCaml to C */ \
        SWITCH_OCAML_TO_C(%r10); \
        ENTER_FUNCTION; \
    /* Call switch stack */ \
        PREPARE_FOR_C_CALL; \
        call    GCALL(caml_switch_stack); \
        CLEANUP_AFTER_C_CALL; \
    /* Load ocaml stack and restore global variables */ \
        LEAVE_FUNCTION; \
        SWITCH_C_TO_OCAML(%r10)


/******************************************************************************/
/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */
/******************************************************************************/

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST (8); \
        pushq   %rbp; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %r12; CFI_ADJUST (8); \
        pushq   %r13; CFI_ADJUST (8); \
        pushq   %r14; CFI_ADJUST (8); \
        pushq   %r15; CFI_ADJUST (8); \
        subq    $(8+10*16), %rsp; CFI_ADJUST (8+10*16); \
        movupd  %xmm6, 0*16(%rsp); \
        movupd  %xmm7, 1*16(%rsp); \
        movupd  %xmm8, 2*16(%rsp); \
        movupd  %xmm9, 3*16(%rsp); \
        movupd  %xmm10, 4*16(%rsp); \
        movupd  %xmm11, 5*16(%rsp); \
        movupd  %xmm12, 6*16(%rsp); \
        movupd  %xmm13, 7*16(%rsp); \
        movupd  %xmm14, 8*16(%rsp); \
        movupd  %xmm15, 9*16(%rsp)

#define POP_CALLEE_SAVE_REGS \
        movupd  0*16(%rsp), %xmm6; \
        movupd  1*16(%rsp), %xmm7; \
        movupd  2*16(%rsp), %xmm8; \
        movupd  3*16(%rsp), %xmm9; \
        movupd  4*16(%rsp), %xmm10; \
        movupd  5*16(%rsp), %xmm11; \
        movupd  6*16(%rsp), %xmm12; \
        movupd  7*16(%rsp), %xmm13; \
        movupd  8*16(%rsp), %xmm14; \
        movupd  9*16(%rsp), %xmm15; \
        addq    $(8+10*16), %rsp; CFI_ADJUST (-8-10*16); \
        popq    %r15; CFI_ADJUST(-8); \
        popq    %r14; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %rdi; CFI_ADJUST(-8); \
        popq    %rsi; CFI_ADJUST(-8); \
        popq    %rbp; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8)

#else

/* Unix API: callee-save regs are rbx, rbp, r12-r15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST(8); \
        pushq   %rbp; CFI_ADJUST(8); \
        pushq   %r12; CFI_ADJUST(8); \
        pushq   %r13; CFI_ADJUST(8); \
        pushq   %r14; CFI_ADJUST(8); \
        pushq   %r15; CFI_ADJUST(8); \
        subq    $8, %rsp; CFI_ADJUST(8)

#define POP_CALLEE_SAVE_REGS \
        addq    $8, %rsp; CFI_ADJUST(-8); \
        popq    %r15; CFI_ADJUST(-8); \
        popq    %r14; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %rbp; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8);

#endif

#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
#  define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
#  define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#endif

/******************************************************************************/
/* Registers holding arguments of C functions. */
/******************************************************************************/

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#define C_ARG_1 %rcx
#define C_ARG_2 %rdx
#define C_ARG_3 %r8
#define C_ARG_4 %r9
#else
#define C_ARG_1 %rdi
#define C_ARG_2 %rsi
#define C_ARG_3 %rdx
#define C_ARG_4 %rcx
#endif

/* Stack-local variables */

#define Stack_sp(REG)               (REG)
#define Stack_dirty(REG)            8(REG)
#define Stack_handle_value(REG)     16(REG)
#define Stack_handle_exception(REG) 24(REG)
#define Stack_handle_effect(REG)    32(REG)
#define Stack_parent(REG)           40(REG)

        .text

        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */

/******************************************************************************/
/* Allocation */
/******************************************************************************/

#define SAVE_ALL_REGS_AND_CALL(TARGET_FUN)                                     \
    /* Build array of registers, save it into OCaml stack's gc_regs slot */    \
        pushq   %r11; CFI_ADJUST (8);                                          \
        pushq   %r10; CFI_ADJUST (8);                                          \
        pushq   %r13; CFI_ADJUST (8);                                          \
        pushq   %r12; CFI_ADJUST (8);                                          \
        pushq   %r9; CFI_ADJUST (8);                                           \
        pushq   %r8; CFI_ADJUST (8);                                           \
        pushq   %rcx; CFI_ADJUST (8);                                          \
        pushq   %rdx; CFI_ADJUST (8);                                          \
        pushq   %rsi; CFI_ADJUST (8);                                          \
        pushq   %rdi; CFI_ADJUST (8);                                          \
        pushq   %rbx; CFI_ADJUST (8);                                          \
        pushq   %rax; CFI_ADJUST (8);                                          \
    /* Update in OCaml stack's context, the pointer to gc_regs */              \
        movq    %rsp, C_ARG_1;                                                 \
				PREPARE_FOR_C_CALL;																										 \
				call 		GCALL(caml_update_gc_regs_slot);															 \
				CLEANUP_AFTER_C_CALL; 																								 \
    /* Save caml_young_ptr */                                                  \
        STORE_TL_VAR(%r15, caml_young_ptr);                                    \
    /* Save floating-point registers */                                        \
        subq    $(16*8), %rsp; CFI_ADJUST (16*8);                              \
        movsd   %xmm0, 0*8(%rsp);                                              \
        movsd   %xmm1, 1*8(%rsp);                                              \
        movsd   %xmm2, 2*8(%rsp);                                              \
        movsd   %xmm3, 3*8(%rsp);                                              \
        movsd   %xmm4, 4*8(%rsp);                                              \
        movsd   %xmm5, 5*8(%rsp);                                              \
        movsd   %xmm6, 6*8(%rsp);                                              \
        movsd   %xmm7, 7*8(%rsp);                                              \
        movsd   %xmm8, 8*8(%rsp);                                              \
        movsd   %xmm9, 9*8(%rsp);                                              \
        movsd   %xmm10, 10*8(%rsp);                                            \
        movsd   %xmm11, 11*8(%rsp);                                            \
        movsd   %xmm12, 12*8(%rsp);                                            \
        movsd   %xmm13, 13*8(%rsp);                                            \
        movsd   %xmm14, 14*8(%rsp);                                            \
        movsd   %xmm15, 15*8(%rsp);                                            \
    /* Call the garbage collector */                                           \
        PREPARE_FOR_C_CALL;                                                    \
    /* Args are relevant only for caml_realloc_stack */                        \
        movq    $0, C_ARG_1;                                                   \
        movq    $0, C_ARG_2;                                                   \
        movq    $0, C_ARG_3;                                                   \
        call    GCALL(TARGET_FUN);                                             \
        CLEANUP_AFTER_C_CALL;                                                  \
    /* Restore caml_young_ptr */                                               \
        LOAD_TL_VAR(caml_young_ptr, %r15);                                     \
    /* Restore all regs used by the code generator */                          \
        movsd   0*8(%rsp), %xmm0;                                              \
        movsd   1*8(%rsp), %xmm1;                                              \
        movsd   2*8(%rsp), %xmm2;                                              \
        movsd   3*8(%rsp), %xmm3;                                              \
        movsd   4*8(%rsp), %xmm4;                                              \
        movsd   5*8(%rsp), %xmm5;                                              \
        movsd   6*8(%rsp), %xmm6;                                              \
        movsd   7*8(%rsp), %xmm7;                                              \
        movsd   8*8(%rsp), %xmm8;                                              \
        movsd   9*8(%rsp), %xmm9;                                              \
        movsd   10*8(%rsp), %xmm10;                                            \
        movsd   11*8(%rsp), %xmm11;                                            \
        movsd   12*8(%rsp), %xmm12;                                            \
        movsd   13*8(%rsp), %xmm13;                                            \
        movsd   14*8(%rsp), %xmm14;                                            \
        movsd   15*8(%rsp), %xmm15;                                            \
        addq    $(16*8), %rsp; CFI_ADJUST(-16*8);                              \
        popq    %rax; CFI_ADJUST(-8);                                          \
        popq    %rbx; CFI_ADJUST(-8);                                          \
        popq    %rdi; CFI_ADJUST(-8);                                          \
        popq    %rsi; CFI_ADJUST(-8);                                          \
        popq    %rdx; CFI_ADJUST(-8);                                          \
        popq    %rcx; CFI_ADJUST(-8);                                          \
        popq    %r8; CFI_ADJUST(-8);                                           \
        popq    %r9; CFI_ADJUST(-8);                                           \
        popq    %r12; CFI_ADJUST(-8);                                          \
        popq    %r13; CFI_ADJUST(-8);                                          \
        popq    %r10; CFI_ADJUST(-8);                                          \
        popq    %r11; CFI_ADJUST(-8)

FUNCTION(G(caml_call_gc))
CFI_STARTPROC
        SWITCH_OCAML_TO_C(%rax)
        ENTER_FUNCTION
        call    LBL(do_gc)
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%rax)
        ret
LBL(do_gc):
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION
#else
        pushq   %rbp; CFI_ADJUST(8);
#endif
        SAVE_ALL_REGS_AND_CALL(caml_garbage_collection)
#ifdef WITH_FRAME_POINTERS
        LEAVE_FUNCTION;
#else
        popq    %rbp; CFI_ADJUST(-8)
#endif

    /* Return to caller */
        ret
CFI_ENDPROC

FUNCTION(G(caml_call_realloc_stack))
CFI_STARTPROC
        SWITCH_OCAML_TO_C(%r10)
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION
#else
        pushq   %rbp; CFI_ADJUST(8);
#endif
        SAVE_ALL_REGS_AND_CALL(caml_realloc_stack)
#ifdef WITH_FRAME_POINTERS
        LEAVE_FUNCTION;
#else
        popq    %rbp; CFI_ADJUST(-8)
#endif
        SWITCH_C_TO_OCAML(%r10)
    /* Return to caller */
        ret
CFI_ENDPROC

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
LBL(caml_alloc1):
        subq    $16, %r15
        CMP_TL_VAR(caml_young_limit, %r15)
        jb      LBL(100)
        ret
LBL(100):
        SWITCH_OCAML_TO_C(%rax)
        ENTER_FUNCTION
        call    LBL(do_gc)
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%rax)
        jmp     LBL(caml_alloc1)
CFI_ENDPROC

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
LBL(caml_alloc2):
        subq    $24, %r15
        CMP_TL_VAR(caml_young_limit, %r15)
        jb      LBL(101)
        ret
LBL(101):
        SWITCH_OCAML_TO_C(%rax)
        ENTER_FUNCTION
        call    LBL(do_gc)
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%rax)
        jmp     LBL(caml_alloc2)
CFI_ENDPROC

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
LBL(caml_alloc3):
        subq    $32, %r15
        CMP_TL_VAR(caml_young_limit, %r15)
        jb      LBL(102)
        ret
LBL(102):
        SWITCH_OCAML_TO_C(%rax)
        ENTER_FUNCTION
        call    LBL(do_gc)
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%rax)
        jmp     LBL(caml_alloc3)
CFI_ENDPROC

FUNCTION(G(caml_allocN))
CFI_STARTPROC
LBL(caml_allocN):
        pushq   %rax; CFI_ADJUST(8)        /* save desired size */
        subq    %rax, %r15
        CMP_TL_VAR(caml_young_limit, %r15)
        jb      LBL(103)
        addq    $8, %rsp; CFI_ADJUST (-8)  /* drop desired size */
        ret
LBL(103):
        /* Save desired size on system stack */
        LOAD_TL_VAR(caml_system_sp, %rax)
        popq    -8(%rax)
        SWITCH_OCAML_TO_C(%rax)
        subq    $16, %rsp
        ENTER_FUNCTION
        call    LBL(do_gc)
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%rax)
        LOAD_TL_VAR(caml_system_sp, %rax)
        movq    -8(%rax), %rax
        jmp     LBL(caml_allocN)
CFI_ENDPROC

/******************************************************************************/
/* Call a C function from OCaml */
/******************************************************************************/

FUNCTION(G(caml_c_call))
CFI_STARTPROC
LBL(caml_c_call):
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C(%r10)
        ENTER_FUNCTION
    /* Make the alloc ptr available to the C code */
        STORE_TL_VAR(%r15, caml_young_ptr)
    /* Call the function (address in %rax) */
        PREPARE_FOR_C_CALL
        call    *%rax
        CLEANUP_AFTER_C_CALL
    /* Prepare for return to OCaml */
        LOAD_TL_VAR(caml_young_ptr, %r15)
    /* Load ocaml stack and restore global variables */
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%r10)
    /* Return to OCaml caller */
        ret
CFI_ENDPROC

FUNCTION(G(caml_c_call_stack_args))
CFI_STARTPROC
    /* Arguments:
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9
        C function          : %rax
        C stack args        : begin=%r13 end=%r12 */
    /* Switch from OCaml to C */
        SWITCH_OCAML_TO_C(%r10)
        ENTER_FUNCTION
    /* Make the alloc ptr available to the C code */
        STORE_TL_VAR(%r15, caml_young_ptr)
    /* Copy arguments from OCaml to C stack */
LBL(105):
        subq    $8, %r12; CFI_ADJUST(8)
        cmpq    %r13,%r12
        jl      LBL(106)
        push    (%r12)
        jmp     LBL(105)
LBL(106):
    /* Call the function (address in %rax) */
        PREPARE_FOR_C_CALL
        call    *%rax
        CLEANUP_AFTER_C_CALL
    /* Prepare for return to OCaml */
        LOAD_TL_VAR(caml_young_ptr, %r15)
    /* Load ocaml stack and restore global variables */
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%r10)
    /* Return to OCaml caller */
        ret
CFI_ENDPROC

/******************************************************************************/
/* Start the OCaml program */
/******************************************************************************/

FUNCTION(G(caml_start_program))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load caml_young_ptr into r15 (was passed as an argument from C) */
        movq    C_ARG_1, %r15
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
        movq    $0, %rax  /* dummy */
        movq    $0, %rbx  /* dummy */
        movq    $0, %rdi  /* dummy */
        movq    $0, %rsi  /* dummy */
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Save system stack state. System stack is captured unalinged. Hence, any
     * OCaml to C calls are expected to explicitly align stack using
     * ENTER_FUNCTION and such. */
        PUSH_TL_VAR(caml_system_exnptr_offset)
        PUSH_TL_VAR(caml_system_sp)
    /* Save the reference to parent stack on C stack and reset it in
     * the OCaml stack */
        LOAD_TL_VAR(caml_current_stack, %r13)
        pushq   Stack_parent(%r13)
        movq    $1, Stack_parent(%r13)
    /* Build a handler for exceptions raised in C */
        lea     LBL(115)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   $0 ; CFI_ADJUST(8)     /* dummy prev trap */
        STORE_TL_VAR(%rsp, caml_system_sp)
    /* Load C exception handler */
        LOAD_TL_VAR(caml_system_stack_high, %r14)
        subq    %rsp, %r14
        STORE_TL_VAR(%r14, caml_system_exnptr_offset)
    /* Save callback args */
        subq    $8, %rsp; CFI_ADJUST(8)
        pushq   %r12; CFI_ADJUST(8)
        pushq   %rax; CFI_ADJUST(8)
        pushq   %rbx; CFI_ADJUST(8)
        pushq   %rdi; CFI_ADJUST(8)
        pushq   %rsi; CFI_ADJUST(8)
    /* Create a register array since caml_maybe_expand_stack may GC */
        movq    %rsp, C_ARG_1
    /* Expand the current stack if needed */
        LEA_VAR(caml_maybe_expand_stack, %r12)
        PREPARE_FOR_C_CALL
        call    *%r12
        CLEANUP_AFTER_C_CALL
    /* Now caml_current_stack corresponds to the new stack. Still on C stack.
     * Restore callback args. */
        popq    %rsi; CFI_ADJUST(-8)
        popq    %rdi; CFI_ADJUST(-8)
        popq    %rbx; CFI_ADJUST(-8)
        popq    %rax; CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)
        addq    $8, %rsp; CFI_ADJUST(-8)
    /* Switch from C to OCaml stack. */
        SWITCH_C_TO_OCAML_NO_CTXT(%r10)
    /* Setup alloc ptr */
        LOAD_TL_VAR(caml_young_ptr, %r15)
    /* Build a handler for exceptions raised in OCaml */
        lea     LBL(109)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   %r14; CFI_ADJUST(8)
        LOAD_TL_VAR(caml_stack_high, %r14); \
        sub     %rsp, %r14
    /* Call the OCaml code */
        call    *%r12
LBL(108):
    /* Pop the OCaml exception handler */
        popq    %r14; CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)   /* dummy register */
        CFI_ADJUST(-16)
LBL(110):
    /* Update alloc ptr */
        STORE_TL_VAR(%r15,caml_young_ptr)
    /* Return to C stack. */
        SWITCH_OCAML_TO_C_NO_CTXT(%r10)
    /* Pop the C exception handler */
        addq    $16, %rsp; CFI_ADJUST(-16)
    /* Restore previous parent stack */
        LOAD_TL_VAR(caml_current_stack, %r10)
        popq    Stack_parent(%r10)
    /* Restore previous system stack state */
        POP_TL_VAR(caml_system_sp)
        POP_TL_VAR(caml_system_exnptr_offset)
    /* Restore callee-save registers. */
        POP_CALLEE_SAVE_REGS
    /* Return to caller. */
        ret
LBL(109):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        jmp     LBL(110)
LBL(115):
    /* Exception in C code */
    /* Reinstall handler for exceptions raised in C */
        lea     LBL(115)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   $0 ; CFI_ADJUST(8)     /* dummy prev trap */
    /* Prepare for return to OCaml */
        LOAD_TL_VAR(caml_young_ptr, %r15)
    /* Load ocaml stack and restore global variables */
        LEAVE_FUNCTION
        SWITCH_C_TO_OCAML(%r10)
    /* Raise the exception in OCaml */
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

/******************************************************************************/
/* Exceptions */
/******************************************************************************/

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
LBL(caml_raise_exn):
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC

FUNCTION(G(caml_reraise_exn))
CFI_STARTPROC
        RESTORE_EXN_HANDLER_OCAML
        ret
CFI_ENDPROC

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        movq    C_ARG_1, %r15                /* young_ptr */
        movq    C_ARG_2, %rax
        LOAD_TL_VAR(caml_system_exnptr_offset, %r14)
        RESTORE_EXN_HANDLER_SYS             /* Cut stack */
        ret
CFI_ENDPROC

/* Raise a Stack_overflow exception on return from segv_handler()
   (in asmrun/signals_asm.c).  On entry, the stack is full, so we
   cannot record a backtrace. */

FUNCTION(G(caml_stack_overflow))
CFI_STARTPROC
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        LOAD_TL_VAR(caml_system_exnptr_offset, %r14)
        RESTORE_EXN_HANDLER_SYS /* cut the stack */
        ret                     /* jump to handler's code */
CFI_ENDPROC

/******************************************************************************/
/* Callback from C to OCaml */
/******************************************************************************/

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r15      /* young_ptr */
        movq    C_ARG_2, %rbx      /* closure */
        movq    C_ARG_3, %rax      /* argument */
        movq    0(%rbx), %r12      /* code pointer */
        movq    $0, %rdi           /* dummy */
        movq    $0, %rsi           /* dummy */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r15      /* young_ptr */
        movq    C_ARG_2, %rdi      /* closure */
        movq    C_ARG_3, %rax      /* first argument */
        movq    C_ARG_4, %rbx      /* second argument */
        LEA_VAR(caml_apply2, %r12) /* code pointer */
        movq    $0, %rsi           /* dummy */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r15      /* young_ptr */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        movq    C_ARG_2, %rsi      /* closure */
        movq    16(C_ARG_3), %rdi  /* third argument */
        LEA_VAR(caml_apply3, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC

/* Fibers */

FUNCTION(G(caml_fiber_exn_handler))
CFI_STARTPROC
    /* In handler. %rax has exception */
        mov     %rax, %rbx
        LOAD_TL_VAR(caml_current_stack, %rsi)
        movq    $0, Stack_sp(%rsi)                  /* zero SP */
        movq    Stack_handle_exception(%rsi), %r12  /* exception handler */
        movq    Stack_parent(%rsi), %rdi            /* parent stack. Never NULL here. */
    /* Reset stack */
        movq    $1, Stack_handle_value(%rsi)
        movq    $1, Stack_handle_exception(%rsi)
        movq    $1, Stack_handle_effect(%rsi)
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke exception handler */
        movq    %rbx, %rax        /* First argument */
        movq    %r12, %rbx        /* Second argument */
        movq    (%rbx), %rsi
        jmp     *%rsi
CFI_ENDPROC

FUNCTION(G(caml_fiber_val_handler))
CFI_STARTPROC
LBL(111):
    /* In handler. %rax has value */
        mov     %rax, %rbx
        LOAD_TL_VAR(caml_current_stack, %rsi)
        movq    $0, Stack_sp(%rsi)                /* zero SP */
        movq    Stack_handle_value(%rsi), %r12    /* value handler */
        movq    Stack_parent(%rsi), %rdi          /* parent stack. Never NULL here. */
    /* Reset stack. First pop off fiber exn handler. */
        popq    %r10
        popq    %r10
        movq    $1, Stack_handle_value(%rsi)
        movq    $1, Stack_handle_exception(%rsi)
        movq    $1, Stack_handle_effect(%rsi)
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke value handler */
        movq    %rbx, %rax        /* First argument */
        movq    %r12, %rbx        /* Second argument */
        movq    (%rbx), %rsi
        jmp     *%rsi
CFI_ENDPROC

FUNCTION(G(caml_perform))
CFI_STARTPROC
    /* In handler. %rax has effect */
        mov     %rax, %r12
        LOAD_TL_VAR(caml_current_stack, %rbx)      /* Second argument */
        movq    Stack_parent(%rbx), %rdi        /* Parent stack. */
        cmpq    $1, %rdi                        /* Parent is NULL? */
        je      LBL(112)
        movq    Stack_handle_effect(%rbx), %r13 /* effect handler (third argument) */
        movq    $1, Stack_parent(%rbx)          /* Set parent stack of performer to NULL */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        movq    %r12, %rax        /* First argument */
        movq    %r13, %rdi        /* Third argument */
        LEA_VAR(caml_apply2, %rsi)
        jmp     *%rsi
LBL(112):
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_exn_Unhandled, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

FUNCTION(G(caml_reperform))
CFI_STARTPROC
    /* %rax -> effect (first argument), %rbx -> performer */
        movq    %rax, %r12
        LOAD_TL_VAR(caml_current_stack, %rax)
        movq    Stack_parent(%rax), %rdi         /* Parent stack */
        cmpq    $1, %rdi                         /* Parent is Null? */
        je      LBL(113)
        movq    %rbx, Stack_parent(%rax)         /* Set performer as parent */
        movq    Stack_handle_effect(%rax), %r13  /* Save effect handler (callee-saved) */
        movq    %rax, %rbx                       /* Save current stack (callee-saved & second argument) */
    /* Switch stacks */
        SWITCH_OCAML_STACKS
    /* Invoke effect handler */
        movq    %r12, %rax                       /* First argument */
        movq    %r13, %rdi                       /* Third argument */
        LEA_VAR(caml_apply2, %rsi)
        jmp     *%rsi
LBL(113):
    /* No parent stack. Raise Unhandled. */
        LEA_VAR(caml_exn_Unhandled, %rax)
        jmp LBL(caml_raise_exn)
CFI_ENDPROC

FUNCTION(G(caml_resume))
CFI_STARTPROC
    /* %rax -> tail of stack list, %rbx -> fun, %rdi -> arg */
        movq    %rdi, %r12      /* Save arg (callee-saved) */
        LOAD_TL_VAR(caml_current_stack, %rcx)
LBL(114):
    /* Rewind parent pointers to find the performer */
        movq    %rax, %rdi
        movq    Stack_parent(%rdi), %rax
        movq    %rcx, Stack_parent(%rdi)
        cmpq    $1, %rax
        movq    %rdi, %rcx
        jne     LBL(114)
    /* %rdi now has the desired head of stack list */
        SWITCH_OCAML_STACKS
        movq    %r12, %rax    /* first argument */
        movq    (%rbx), %rsi  /* closure in %rbx (second argument) */
        jmp     *%rsi
CFI_ENDPROC


FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC

        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   2           /* two descriptors */
        .quad   LBL(108)    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN
        .quad   LBL(111)    /* return address into fiber_val_handler */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst8,"a",@progbits
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
        .quad   0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
        .quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
