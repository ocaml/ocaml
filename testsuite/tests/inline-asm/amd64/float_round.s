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
	.quad	2045
camlFloat_round__1:
	.quad	0x3ff8000000000000
	.data
	.quad	2045
camlFloat_round__2:
	.quad	0x3ff0000000000000
	.data
	.quad	2045
camlFloat_round__3:
	.quad	0xbfe0000000000000
	.data
	.quad	2045
camlFloat_round__4:
	.quad	0xbff0000000000000
	.data
	.quad	2045
camlFloat_round__5:
	.quad	0x4000000000000000
	.data
	.quad	2045
camlFloat_round__6:
	.quad	0
	.data
	.quad	3068
camlFloat_round__7:
	.ascii	"float_round.ml"
	.space	1
	.byte	1
	.data
	.quad	3840
camlFloat_round__8:
	.quad	camlFloat_round__7
	.quad	15
	.quad	5
	.text
	.align	16
	.globl	camlFloat_round__entry
camlFloat_round__entry:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L102:
	movsd	.L103(%rip), %xmm0
	movsd	.L104(%rip), %xmm1
	roundsd	$1, %xmm1, %xmm2
	ucomisd	%xmm0, %xmm2
	jp	.L101
	jne	.L101
	roundsd	$1, %xmm0, %xmm2
	ucomisd	%xmm0, %xmm2
	jp	.L101
	jne	.L101
	movsd	.L105(%rip), %xmm2
	movsd	.L106(%rip), %xmm3
	roundsd	$1, %xmm3, %xmm4
	ucomisd	%xmm2, %xmm4
	jp	.L101
	jne	.L101
	movsd	.L107(%rip), %xmm2
	roundsd	$2, %xmm1, %xmm1
	ucomisd	%xmm2, %xmm1
	jp	.L101
	jne	.L101
	roundsd	$2, %xmm0, %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L101
	jne	.L101
	xorpd	%xmm0, %xmm0
	movapd	%xmm3, %xmm1
	roundsd	$2, %xmm1, %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L108
	je	.L100
.L108:
.L101:
	call	caml_alloc2@PLT
.L109:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlFloat_round__8@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L100:
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
.L107:
	.quad	0x4000000000000000
.L106:
	.quad	0xbfe0000000000000
.L105:
	.quad	0xbff0000000000000
.L104:
	.quad	0x3ff8000000000000
.L103:
	.quad	0x3ff0000000000000
	.text
	.globl	camlFloat_round__code_end
camlFloat_round__code_end:
	.data
	.globl	camlFloat_round__data_end
camlFloat_round__data_end:
	.long	0
	.globl	camlFloat_round__frametable
camlFloat_round__frametable:
	.quad	1
	.quad	.L109
	.word	16
	.word	0
	.align	8
	.section .note.GNU-stack,"",%progbits
