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

#if defined(MODEL_ppc64) || defined(MODEL_ppc64le)
#define EITHER(a,b) b
#else
#define EITHER(a,b) a
#endif

#define WORD EITHER(4,8)
#define lg EITHER(lwz,ld)
#define lgu EITHER(lwzu,ldu)
#define stg EITHER(stw,std)
#define stgu EITHER(stwu,stdu)

#if defined(MODEL_ppc)
#define RESERVED_STACK 16
#define LR_SAVE_AREA 4
#endif
#if defined(MODEL_ppc64)
#define RESERVED_STACK 48
#define LR_SAVE_AREA 16
#endif
#if defined(MODEL_ppc64le)
#define RESERVED_STACK 32
#define LR_SAVE_AREA 16
#endif

/* Function definitions */

#if defined(MODEL_ppc)
#define FUNCTION(name) \
  .section ".text"; \
  .globl name; \
  .type name, @function; \
  .align 2; \
  name:
#endif

#if defined(MODEL_ppc64)
#define FUNCTION(name) \
  .section ".opd","aw"; \
  .align 3; \
  .globl name; \
  .type name, @function; \
  name: .quad .L.name,.TOC.@tocbase; \
  .text; \
  .align 2; \
  .L.name:
#endif

#if defined(MODEL_ppc64le)
#define FUNCTION(name) \
  .section ".text"; \
  .globl name; \
  .type name, @function; \
  .align 2; \
  name: ; \
  0: addis 2, 12, (.TOC. - 0b)@ha; \
  addi 2, 2, (.TOC. - 0b)@l; \
  .localentry name, . - 0b
#endif

FUNCTION(call_gen_code)
    /* Allocate and link stack frame */
        stgu    1, -(WORD*18 + 8*18 + RESERVED_STACK)(1)
    /* 18 saved GPRs, 18 saved FPRs */
    /* Save return address */
        mflr    0
        stg     0, (WORD*18 + 8*18 + RESERVED_STACK + LR_SAVE_AREA)(1)
    /* Save all callee-save registers, starting at RESERVED_STACK */
        addi    11, 1, RESERVED_STACK - WORD
        stgu    14, WORD(11)
        stgu    15, WORD(11)
        stgu    16, WORD(11)
        stgu    17, WORD(11)
        stgu    18, WORD(11)
        stgu    19, WORD(11)
        stgu    20, WORD(11)
        stgu    21, WORD(11)
        stgu    22, WORD(11)
        stgu    23, WORD(11)
        stgu    24, WORD(11)
        stgu    25, WORD(11)
        stgu    26, WORD(11)
        stgu    27, WORD(11)
        stgu    28, WORD(11)
        stgu    29, WORD(11)
        stgu    30, WORD(11)
        stgu    31, WORD(11)
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
#if defined(MODEL_ppc)
        mtctr   3
#elif defined(MODEL_ppc64)
        ld      0, 0(3)
        mtctr   0
        ld      2, 8(3)
#elif defined(MODEL_ppc64le)
        mtctr   3
        mr      12, 3
#else
#error "wrong MODEL"
#endif
    /* Shuffle arguments */
        mr      3, 4
        mr      4, 5
        mr      5, 6
        mr      6, 7
    /* Call the function */
        bctrl
    /* Restore callee-save registers */
        addi    11, 1, RESERVED_STACK - WORD
        lgu     14, WORD(11)
        lgu     15, WORD(11)
        lgu     16, WORD(11)
        lgu     17, WORD(11)
        lgu     18, WORD(11)
        lgu     19, WORD(11)
        lgu     20, WORD(11)
        lgu     21, WORD(11)
        lgu     22, WORD(11)
        lgu     23, WORD(11)
        lgu     24, WORD(11)
        lgu     25, WORD(11)
        lgu     26, WORD(11)
        lgu     27, WORD(11)
        lgu     28, WORD(11)
        lgu     29, WORD(11)
        lgu     30, WORD(11)
        lgu     31, WORD(11)
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
        lg      0, (WORD*18 + 8*18 + RESERVED_STACK + LR_SAVE_AREA)(1)
        mtlr    0
    /* Return */
        addi    1, 1, (WORD*18 + 8*18 + RESERVED_STACK)
        blr

FUNCTION(caml_c_call)
    /* Jump to C function (address in r28) */
#if defined(MODEL_ppc)
        mtctr   28
#elif defined(MODEL_ppc64)
        ld      0, 0(28)
        mtctr   0
        ld      2, 8(28)
#elif defined(MODEL_ppc64le)
        mtctr   28
        mr      12, 28
#else
#error "wrong MODEL"
#endif
        bctr

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
