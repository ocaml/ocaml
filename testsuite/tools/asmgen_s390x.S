#define ALIGN 8

#define CALL_GEN_CODE call_gen_code
#define CAML_C_CALL caml_c_call
#define CAML_NEGF_MASK caml_negf_mask
#define CAML_ABSF_MASK caml_absf_mask

        .section ".text"

        .globl  CALL_GEN_CODE
        .type   CALL_GEN_CODE, @function
        .align  ALIGN
CALL_GEN_CODE:
        /* Stack space */
        lay     %r15, -144(%r15)
        /* Save registers */
        stmg    %r6,%r14, 0(%r15)
        std     %f8, 72(%r15)
        std     %f9, 80(%r15)
        std     %f10, 88(%r15)
        std     %f11, 96(%r15)
        std     %f12, 104(%r15)
        std     %f13, 112(%r15)
        std     %f14, 120(%r15)
        std     %f15, 128(%r15)
        /* Shuffle args */
        lgr     %r1, %r2
        lgr     %r2, %r3
        lgr     %r3, %r4
        lgr     %r4, %r5
        /* Function call */
        basr    %r14, %r1
        /* Restore registers */
        lmg    %r6,%r14, 0(%r15)
        ld     %f8, 72(%r15)
        ld     %f9, 80(%r15)
        ld     %f10, 88(%r15)
        ld     %f11, 96(%r15)
        ld     %f12, 104(%r15)
        ld     %f13, 112(%r15)
        ld     %f14, 120(%r15)
        ld     %f15, 128(%r15)
        /* Return */
        lay     %r15, 144(%r15)
        br      %r14

        .globl  CAML_C_CALL
        .type   CAML_C_CALL, @function
        .align  ALIGN
CAML_C_CALL:
        br      %r7

        .section ".rodata"

        .global CAML_NEGF_MASK
        .align  ALIGN
CAML_NEGF_MASK:
        .quad   0x8000000000000000, 0
        .global  CAML_ABSF_MASK
        .align  ALIGN
CAML_ABSF_MASK:
        .quad   0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8

/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
