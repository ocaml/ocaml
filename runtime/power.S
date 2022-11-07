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

#if defined(MODEL_ppc64le)
        .abiversion 2
#endif

/* Special registers */
#define START_PRG_ARG 12
#define START_PRG_DOMAIN_STATE_PTR 7
#define C_CALL_FUN 25
#define C_CALL_TOC 26
#define C_CALL_RET_ADDR 27
#define TRAP_PTR 29
#define DOMAIN_STATE_PTR 30
#define ALLOC_PTR 31

/* Stack layout */
#define RESERVED_STACK 32
#define PARAM_SAVE_AREA 0
#define LR_SAVE 16
#define TOC_SAVE_PARENT 8
#define TOC_SAVE 24
#define TRAP_SIZE 32
#define TRAP_HANDLER_OFFSET 40
#define TRAP_PREVIOUS_OFFSET 48
#define CALLBACK_LINK_SIZE 32
#define CALLBACK_LINK_OFFSET 32

/* Function definitions */

#define FUNCTION(name) \
  .section ".text"; \
  .globl name; \
  .type name, @function; \
  .align 2; \
  name: ; \
  0: addis 2, 12, (.TOC. - 0b)@ha; \
  addi 2, 2, (.TOC. - 0b)@l; \
  .localentry name, . - 0b

#define ENDFUNCTION(name) \
  .size name, . - name

/* Accessing global variables.  */

#define LSYMB(glob) .L##glob

#define Addrglobal(reg,glob) \
        ld      reg, LSYMB(glob)@toc(2)

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) 8*domain_field_caml_##var(DOMAIN_STATE_PTR)

        .section ".text"
        .globl  caml_system__code_begin
caml_system__code_begin:

/* Invoke the garbage collector. */

FUNCTION(caml_call_gc)
#define STACKSIZE (8*32 + 8*32 + PARAM_SAVE_AREA + RESERVED_STACK)
    /* 32 integer registers + 32 float registers + space for C call */
    /* Set up stack frame */
        stwu    1, -STACKSIZE(1)
    /* Record return address into OCaml code */
        mflr    0
        std     0, Caml_state(last_return_address)
    /* Record lowest stack address */
        addi    0, 1, STACKSIZE
        std     0, Caml_state(bottom_of_stack)
    /* Record pointer to register array */
        addi    0, 1, 8*32 + PARAM_SAVE_AREA + RESERVED_STACK
        std     0, Caml_state(gc_regs)
    /* Save current allocation pointer for debugging purposes */
        std     ALLOC_PTR, Caml_state(young_ptr)
    /* Save exception pointer (if e.g. a sighandler raises) */
        std     TRAP_PTR, Caml_state(exception_pointer)
    /* Save all registers used by the code generator */
        addi    11, 1, 8*32 + PARAM_SAVE_AREA + RESERVED_STACK - 8
        stdu    3, 8(11)
        stdu    4, 8(11)
        stdu    5, 8(11)
        stdu    6, 8(11)
        stdu    7, 8(11)
        stdu    8, 8(11)
        stdu    9, 8(11)
        stdu    10, 8(11)
        stdu    14, 8(11)
        stdu    15, 8(11)
        stdu    16, 8(11)
        stdu    17, 8(11)
        stdu    18, 8(11)
        stdu    19, 8(11)
        stdu    20, 8(11)
        stdu    21, 8(11)
        stdu    22, 8(11)
        stdu    23, 8(11)
        stdu    24, 8(11)
        stdu    25, 8(11)
        stdu    26, 8(11)
        stdu    27, 8(11)
        stdu    28, 8(11)
        addi    11, 1, PARAM_SAVE_AREA + RESERVED_STACK - 8
        stfdu   1, 8(11)
        stfdu   2, 8(11)
        stfdu   3, 8(11)
        stfdu   4, 8(11)
        stfdu   5, 8(11)
        stfdu   6, 8(11)
        stfdu   7, 8(11)
        stfdu   8, 8(11)
        stfdu   9, 8(11)
        stfdu   10, 8(11)
        stfdu   11, 8(11)
        stfdu   12, 8(11)
        stfdu   13, 8(11)
        stfdu   14, 8(11)
        stfdu   15, 8(11)
        stfdu   16, 8(11)
        stfdu   17, 8(11)
        stfdu   18, 8(11)
        stfdu   19, 8(11)
        stfdu   20, 8(11)
        stfdu   21, 8(11)
        stfdu   22, 8(11)
        stfdu   23, 8(11)
        stfdu   24, 8(11)
        stfdu   25, 8(11)
        stfdu   26, 8(11)
        stfdu   27, 8(11)
        stfdu   28, 8(11)
        stfdu   29, 8(11)
        stfdu   30, 8(11)
        stfdu   31, 8(11)
    /* Call the GC */
        bl      caml_garbage_collection
        nop
    /* Reload new allocation pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Restore all regs used by the code generator */
        addi    11, 1, 8*32 + PARAM_SAVE_AREA + RESERVED_STACK - 8
        ldu     3, 8(11)
        ldu     4, 8(11)
        ldu     5, 8(11)
        ldu     6, 8(11)
        ldu     7, 8(11)
        ldu     8, 8(11)
        ldu     9, 8(11)
        ldu     10, 8(11)
        ldu     14, 8(11)
        ldu     15, 8(11)
        ldu     16, 8(11)
        ldu     17, 8(11)
        ldu     18, 8(11)
        ldu     19, 8(11)
        ldu     20, 8(11)
        ldu     21, 8(11)
        ldu     22, 8(11)
        ldu     23, 8(11)
        ldu     24, 8(11)
        ldu     25, 8(11)
        ldu     26, 8(11)
        ldu     27, 8(11)
        ldu     28, 8(11)
        addi    11, 1, PARAM_SAVE_AREA + RESERVED_STACK - 8
        lfdu    1, 8(11)
        lfdu    2, 8(11)
        lfdu    3, 8(11)
        lfdu    4, 8(11)
        lfdu    5, 8(11)
        lfdu    6, 8(11)
        lfdu    7, 8(11)
        lfdu    8, 8(11)
        lfdu    9, 8(11)
        lfdu    10, 8(11)
        lfdu    11, 8(11)
        lfdu    12, 8(11)
        lfdu    13, 8(11)
        lfdu    14, 8(11)
        lfdu    15, 8(11)
        lfdu    16, 8(11)
        lfdu    17, 8(11)
        lfdu    18, 8(11)
        lfdu    19, 8(11)
        lfdu    20, 8(11)
        lfdu    21, 8(11)
        lfdu    22, 8(11)
        lfdu    23, 8(11)
        lfdu    24, 8(11)
        lfdu    25, 8(11)
        lfdu    26, 8(11)
        lfdu    27, 8(11)
        lfdu    28, 8(11)
        lfdu    29, 8(11)
        lfdu    30, 8(11)
        lfdu    31, 8(11)
    /* Return to caller, resuming the allocation */
        ld      11, Caml_state(last_return_address)
        mtlr    11
    /* Restore the TOC that the caller saved at the usual place */
        ld      2, (STACKSIZE + TOC_SAVE)(1)
    /* Deallocate stack frame */
        addi    1, 1, STACKSIZE
        blr
#undef STACKSIZE
ENDFUNCTION(caml_call_gc)

/* Call a C function from OCaml */

FUNCTION(caml_c_call)
        .cfi_startproc
    /* Save return address in a callee-save register */
        mflr    C_CALL_RET_ADDR
        .cfi_register 65, C_CALL_RET_ADDR
    /* Record lowest stack address and return address */
        std     1, Caml_state(bottom_of_stack)
        std     C_CALL_RET_ADDR, Caml_state(last_return_address)
    /* Make the exception handler and alloc ptr available to the C code */
        std     ALLOC_PTR, Caml_state(young_ptr)
        std     TRAP_PTR, Caml_state(exception_pointer)
    /* Call C function (address in C_CALL_FUN) */
        mtctr   C_CALL_FUN
        mr      12, C_CALL_FUN
        mr      C_CALL_TOC, 2   /* save current TOC in a callee-save register */
        bctrl
        mr      2, C_CALL_TOC   /* restore current TOC */
    /* Restore return address (in 27, preserved by the C function) */
        mtlr    C_CALL_RET_ADDR
    /* Reload allocation pointer*/
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Return to caller */
        blr
        .cfi_endproc
ENDFUNCTION(caml_c_call)

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        ld      0, Caml_state(backtrace_active)
        cmpwi   0, 0
        bne     .L111
.L110:
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(TRAP_PTR)
        mr      1, TRAP_PTR
        mtctr   0
        ld      TRAP_PTR, TRAP_PREVIOUS_OFFSET(1)
        addi    1, 1, TRAP_SIZE
    /* Branch to handler */
        bctr
.L111:
        mr      27, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        mflr    4               /* arg2: PC of raise */
        mr      5, 1            /* arg3: SP of raise */
        mr      6, TRAP_PTR     /* arg4: SP of handler */
        addi    1, 1, -(PARAM_SAVE_AREA + RESERVED_STACK)
                                /* reserve stack space for C call */
        bl      caml_stash_backtrace
        nop
        mr      3, 27           /* restore exn bucket */
        b       .L110           /* raise the exn */
ENDFUNCTION(caml_raise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
    /* Load domain state pointer */
        mr      DOMAIN_STATE_PTR, 3
        mr      3, 4
        ld      0, Caml_state(backtrace_active)
        cmpwi   0, 0
        bne     .L121
.L120:
    /* Reload OCaml global registers */
        ld      1, Caml_state(exception_pointer)
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Pop trap frame */
        ld      0, TRAP_HANDLER_OFFSET(1)
        mtctr   0
        ld      TRAP_PTR, TRAP_PREVIOUS_OFFSET(1)
        addi    1, 1, TRAP_SIZE
    /* Branch to handler */
        bctr
.L121:
        mr      27, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        ld      4, Caml_state(last_return_address) /* arg2: PC of raise */
        ld      5, Caml_state(bottom_of_stack)     /* arg3: SP of raise */
        ld      6, Caml_state(exception_pointer)   /* arg4: SP of handler */
        addi    1, 1, -(PARAM_SAVE_AREA + RESERVED_STACK)
                                         /* reserve stack space for C call */
        bl      caml_stash_backtrace
        nop
        mr      3, 27           /* restore exn bucket */
        b       .L120           /* raise the exn */
ENDFUNCTION(caml_raise_exception)

/* Start the OCaml program */

FUNCTION(caml_start_program)
        .cfi_startproc
#define STACKSIZE (8*18 + 8*18 + CALLBACK_LINK_SIZE + RESERVED_STACK)
  /* 18 callee-save GPR14...GPR31 + 18 callee-save FPR14...FPR31 */
  /* Domain state pointer is the first arg to caml_start_program. Move it */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        Addrglobal(START_PRG_ARG, caml_program)
/* Code shared between caml_start_program and caml_callback */
.L102:
    /* Allocate and link stack frame */
        stdu    1, -STACKSIZE(1)
        .cfi_adjust_cfa_offset STACKSIZE
    /* Save return address */
        mflr    0
        std     0, (STACKSIZE + LR_SAVE)(1)
        .cfi_offset 65, LR_SAVE
    /* Save TOC pointer */
        std     2, (STACKSIZE + TOC_SAVE_PARENT)(1)
    /* Save all callee-save registers */
        addi    11, 1, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        stdu    14, 8(11)
        stdu    15, 8(11)
        stdu    16, 8(11)
        stdu    17, 8(11)
        stdu    18, 8(11)
        stdu    19, 8(11)
        stdu    20, 8(11)
        stdu    21, 8(11)
        stdu    22, 8(11)
        stdu    23, 8(11)
        stdu    24, 8(11)
        stdu    25, 8(11)
        stdu    26, 8(11)
        stdu    27, 8(11)
        stdu    28, 8(11)
        stdu    29, 8(11)
        stdu    30, 8(11)
        stdu    31, 8(11)
        stfdu   14, 8(11)
        stfdu   15, 8(11)
        stfdu   16, 8(11)
        stfdu   17, 8(11)
        stfdu   18, 8(11)
        stfdu   19, 8(11)
        stfdu   20, 8(11)
        stfdu   21, 8(11)
        stfdu   22, 8(11)
        stfdu   23, 8(11)
        stfdu   24, 8(11)
        stfdu   25, 8(11)
        stfdu   26, 8(11)
        stfdu   27, 8(11)
        stfdu   28, 8(11)
        stfdu   29, 8(11)
        stfdu   30, 8(11)
        stfdu   31, 8(11)
    /* Load domain state pointer from argument */
        mr      DOMAIN_STATE_PTR, START_PRG_DOMAIN_STATE_PTR
    /* Set up a callback link */
        ld      11, Caml_state(bottom_of_stack)
        std     11, CALLBACK_LINK_OFFSET(1)
        ld      11, Caml_state(last_return_address)
        std     11, (CALLBACK_LINK_OFFSET + 8)(1)
        ld      11, Caml_state(gc_regs)
        std     11, (CALLBACK_LINK_OFFSET + 2 * 8)(1)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        bl      .L103
        b       .L104
.L103:
        addi    1, 1, -TRAP_SIZE
        .cfi_adjust_cfa_offset TRAP_SIZE
        mflr    0
        std     0, TRAP_HANDLER_OFFSET(1)
        ld      11, Caml_state(exception_pointer)
        std     11, TRAP_PREVIOUS_OFFSET(1)
        mr      TRAP_PTR, 1
    /* Reload allocation pointer */
        ld      ALLOC_PTR, Caml_state(young_ptr)
    /* Call the OCaml code (address in r12) */
        mtctr   12
        std     2, TOC_SAVE(1)
.L105:  bctrl
        ld      2, TOC_SAVE(1)
    /* Pop the trap frame, restoring caml_exception_pointer */
        ld      0, TRAP_PREVIOUS_OFFSET(1)
        std     0, Caml_state(exception_pointer)
        addi    1, 1, TRAP_SIZE
        .cfi_adjust_cfa_offset -TRAP_SIZE
    /* Pop the callback link, restoring the global variables */
.L106:
        ld      0, CALLBACK_LINK_OFFSET(1)
        std     0, Caml_state(bottom_of_stack)
        ld      0, (CALLBACK_LINK_OFFSET + 8)(1)
        std     0, Caml_state(last_return_address)
        ld      0, (CALLBACK_LINK_OFFSET + 2 * 8)(1)
        std     0, Caml_state(gc_regs)
    /* Update allocation pointer */
        std     ALLOC_PTR, Caml_state(young_ptr)
    /* Restore callee-save registers */
        addi    11, 1, CALLBACK_LINK_SIZE + RESERVED_STACK - 8
        ldu     14, 8(11)
        ldu     15, 8(11)
        ldu     16, 8(11)
        ldu     17, 8(11)
        ldu     18, 8(11)
        ldu     19, 8(11)
        ldu     20, 8(11)
        ldu     21, 8(11)
        ldu     22, 8(11)
        ldu     23, 8(11)
        ldu     24, 8(11)
        ldu     25, 8(11)
        ldu     26, 8(11)
        ldu     27, 8(11)
        ldu     28, 8(11)
        ldu     29, 8(11)
        ldu     30, 8(11)
        ldu     31, 8(11)
        lfdu    14, 8(11)
        lfdu    15, 8(11)
        lfdu    16, 8(11)
        lfdu    17, 8(11)
        lfdu    18, 8(11)
        lfdu    19, 8(11)
        lfdu    20, 8(11)
        lfdu    21, 8(11)
        lfdu    22, 8(11)
        lfdu    23, 8(11)
        lfdu    24, 8(11)
        lfdu    25, 8(11)
        lfdu    26, 8(11)
        lfdu    27, 8(11)
        lfdu    28, 8(11)
        lfdu    29, 8(11)
        lfdu    30, 8(11)
        lfdu    31, 8(11)
    /* Reload return address */
        ld      0, (STACKSIZE + LR_SAVE)(1)
        mtlr    0
    /* Return */
        addi    1, 1, STACKSIZE
        blr

    /* The trap handler: */
.L104:
    /* Restore TOC pointer */
        ld      2, (STACKSIZE + TOC_SAVE_PARENT)(1)
    /* Update caml_exception_pointer */
        std     TRAP_PTR, Caml_state(exception_pointer)
    /* Encode exception bucket as an exception result and return it */
        ori     3, 3, 2
        b       .L106
#undef STACKSIZE
        .cfi_endproc
ENDFUNCTION(caml_start_program)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
    /* Initial shuffling of arguments */
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        ld      3, 0(5)             /* r3 = Argument */
                                    /* r4 = Closure */
        ld      START_PRG_ARG, 0(4) /* Code pointer */
        b       .L102
ENDFUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg,
       8(r5) = second arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      0, 4
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        mr      5, 0                /* r5 = Closure */
        Addrglobal(START_PRG_ARG, caml_apply2)
        b       .L102
ENDFUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
    /* r3 = Caml_state, r4 = closure, 0(r5) = first arg, 8(r5) = second arg,
       2*8(r5) = third arg */
        mr      START_PRG_DOMAIN_STATE_PTR, 3
        mr      6, 4                /* r6 = Closure */
        ld      3, 0(5)             /* r3 = First argument */
        ld      4, 8(5)          /* r4 = Second argument */
        ld      5, 2*8(5)        /* r5 = Third argument */
        Addrglobal(START_PRG_ARG, caml_apply3)
        b       .L102
ENDFUNCTION(caml_callback3_asm)

        .section ".text"
        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .globl  caml_system.frametable
        .type   caml_system.frametable, @object
caml_system__frametable:
        .quad   1               /* one descriptor */
        .quad   .L105 + 4       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */

/* TOC entries */

        .section ".toc", "aw"

#define TOCENTRY(glob) LSYMB(glob): .quad glob

TOCENTRY(caml_apply2)
TOCENTRY(caml_apply3)
TOCENTRY(caml_program)

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
