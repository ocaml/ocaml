	.section .rodata.cst8,"a",@progbits
	.align	16
caml_negf_mask:
	.quad	0x8000000000000000
	.quad	0
	.align	16
caml_absf_mask:
	.quad	0x7fffffffffffffff
	.quad	-1
	.data
	.globl	camlFloat_round__data_begin
camlFloat_round__data_begin:
	.text
	.globl	camlFloat_round__code_begin
camlFloat_round__code_begin:
	.data
	.quad	768
	.globl	camlFloat_round
camlFloat_round:
	.data
	.quad	4094
camlFloat_round__4:
	.quad	0x3ff8000000000000
	.quad	0x3ff0000000000000
	.quad	0xbfe0000000000000
	.data
	.quad	2045
camlFloat_round__5:
	.quad	0x3ff0000000000000
	.data
	.quad	2045
camlFloat_round__6:
	.quad	0xbff0000000000000
	.data
	.quad	2045
camlFloat_round__7:
	.quad	0x4000000000000000
	.data
	.quad	2045
camlFloat_round__8:
	.quad	0
	.data
	.quad	3068
camlFloat_round__9:
	.ascii	"float_round.ml"
	.space	1
	.byte	1
	.data
	.quad	3840
camlFloat_round__10:
	.quad	camlFloat_round__9
	.quad	25
	.quad	5
	.text
	.align	16
	.globl	camlFloat_round__entry
camlFloat_round__entry:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L108:
	movsd	.L109(%rip), %xmm0
	movq	camlFloat_round__4@GOTPCREL(%rip), %rax
	roundsd	$1, (%rax), %xmm1	# floor
	ucomisd	%xmm0, %xmm1
	jp	.L107
	jne	.L107
	roundsd	$1, 8(%rax), %xmm1	# floor
	ucomisd	%xmm0, %xmm1
	jp	.L107
	jne	.L107
	movsd	.L110(%rip), %xmm1
	roundsd	$1, 16(%rax), %xmm2	# floor
	ucomisd	%xmm1, %xmm2
	jp	.L107
	jne	.L107
	movsd	.L111(%rip), %xmm1
	roundsd	$2, (%rax), %xmm2	# ceil
	ucomisd	%xmm1, %xmm2
	jp	.L107
	jne	.L107
	roundsd	$2, 8(%rax), %xmm1	# ceil
	ucomisd	%xmm0, %xmm1
	jp	.L107
	jne	.L107
	xorpd	%xmm0, %xmm0
	roundsd	$2, 16(%rax), %xmm1	# ceil
	ucomisd	%xmm0, %xmm1
	jp	.L112
	je	.L106
.L112:
.L107:
	call	caml_alloc2@PLT
.L113:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlFloat_round__10@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L106:
	movq	$1, %rax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.cfi_endproc
	.type camlFloat_round__entry,@function
	.size camlFloat_round__entry,. - camlFloat_round__entry
	.data
	.section .rodata.cst8,"a",@progbits
.L111:
	.quad	0x4000000000000000
.L110:
	.quad	0xbff0000000000000
.L109:
	.quad	0x3ff0000000000000
	.text
	.globl	camlFloat_round__code_end
camlFloat_round__code_end:
	.data
				/* relocation table start */
	.align	8
				/* relocation table end */
	.data
	.globl	camlFloat_round__data_end
camlFloat_round__data_end:
	.long	0
	.globl	camlFloat_round__frametable
camlFloat_round__frametable:
	.quad	1
	.quad	.L113
	.word	16
	.word	0
	.align	8
	.section .note.GNU-stack,"",%progbits
