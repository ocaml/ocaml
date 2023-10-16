/*********************************************************************/
/*                                                                   */
/*                               OCaml                               */
/*                                                                   */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        */
/*                                                                   */
/* Copyright 1996 Institut National de Recherche en Informatique et  */
/* en Automatique.  All rights reserved.  This file is distributed   */
/* under the terms of the Q Public License version 1.0.              */
/*                                                                   */
/*********************************************************************/

#define RESERVED_STACK 32
#define LR_SAVE_AREA 16

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

FUNCTION(call_gen_code)
    /* Allocate and link stack frame */
        stdu    1, -(8*18 + 8*18 + RESERVED_STACK)(1)
    /* 18 saved GPRs, 18 saved FPRs */
    /* Save return address */
        mflr    0
        std     0, (8*18 + 8*18 + RESERVED_STACK + LR_SAVE_AREA)(1)
    /* Save all callee-save registers, starting at RESERVED_STACK */
        addi    11, 1, RESERVED_STACK - 8
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
    /* Get function pointer in CTR */
        mtctr   3
        mr      12, 3
    /* Shuffle arguments */
        mr      3, 4
        mr      4, 5
        mr      5, 6
        mr      6, 7
    /* Call the function */
        bctrl
    /* Restore callee-save registers */
        addi    11, 1, RESERVED_STACK - 8
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
        ld      0, (8*18 + 8*18 + RESERVED_STACK + LR_SAVE_AREA)(1)
        mtlr    0
    /* Return */
        addi    1, 1, (8*18 + 8*18 + RESERVED_STACK)
        blr

FUNCTION(caml_c_call)
    /* Jump to C function (address in r28) */
        mtctr   28
        mr      12, 28
        bctr

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
