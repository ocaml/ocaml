#define ALIGN 8

#define CALL_GEN_CODE call_gen_code
#define CAML_C_CALL caml_c_call
#define CAML_NEGF_MASK caml_negf_mask
#define CAML_ABSF_MASK caml_absf_mask

	.section "text"
	
	.globl	CALL_GEN_CODE
	.type	CALL_GEN_CODE, @function
	.align	ALIGN
CALL_GEN_CODE:
	/* Stack space */
	lay	%r15, -48(%r15)
	/* Save registers */
	stg	%r14, 0(%r15)
	stg	%r7, 8(%r15)
	stg	%r8, 16(%r15)
	stg	%r9, 24(%r15)
	stg	%r10, 32(%r15)
	stg	%r11, 40(%r15)
	/* Shuffle args */
	lgr	%r1, %r2
	lgr	%r2, %r3
	lgr	%r3, %r4
	lgr	%r4, %r5
	/* Function call */
	brasl	%r14, %r1
	/* Restore registers */
	lg	%r11, 40(%r15)
	lg	%r10, 32(%r15)
	lg	%r9, 24(%r15)
	lg	%r8, 16(%r15)
	lg	%r7, 8(%r15)


	.globl	CAML_C_CALL
	.type	CAML_C_CALL, @function
	.align	ALIGN
CAML_C_CALL:
	lay	%r15, -160(%r15)
        brasl %r14, %r1
	lay	%r15, 160(%r15)
	
	.section	.rodata

	.global	CAML_NEGF_MASK
	.align	ALIGN
CAML_NEGF_MASK:
	.quad	0x8000000000000000, 0
	.global  CAML_ABSF_MASK
        .align  ALIGN
CAML_ABSF_MASK:
        .quad   0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8