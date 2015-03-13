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
	.quad	3068
camlFloat_round__3:
	.ascii	"float_round.ml"
	.space	1
	.byte	1
	.data
	.quad	3840
camlFloat_round__4:
	.quad	camlFloat_round__3
	.quad	15
	.quad	5
	.data
	.quad	3068
camlFloat_round__5:
	.ascii	"float_round.ml"
	.space	1
	.byte	1
	.data
	.quad	3840
camlFloat_round__6:
	.quad	camlFloat_round__5
	.quad	17
	.quad	5
	.data
	.quad	2045
camlFloat_round__7:
	.quad	0xbfe0000000000000
	.data
	.quad	2045
camlFloat_round__8:
	.quad	0xbff0000000000000
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
	.quad	19
	.quad	5
	.text
	.align	16
	.globl	camlFloat_round__entry
camlFloat_round__entry:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L103:
	movsd	.L104(%rip), %xmm0
	movsd	.L105(%rip), %xmm1
	roundsd	$1, %xmm1, %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L106
	je	.L102
.L106:
	call	caml_alloc2@PLT
.L107:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlFloat_round__4@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L102:
	movsd	.L104(%rip), %xmm0
	roundsd	$1, %xmm0, %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L108
	je	.L101
.L108:
	call	caml_alloc2@PLT
.L109:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlFloat_round__6@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L101:
	movsd	.L110(%rip), %xmm0
	movsd	.L111(%rip), %xmm1
	roundsd	$1, %xmm1, %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L112
	je	.L100
.L112:
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
.L111:
	.quad	0xbfe0000000000000
.L110:
	.quad	0xbff0000000000000
.L105:
	.quad	0x3ff8000000000000
.L104:
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
	.quad	3
	.quad	.L113
	.word	16
	.word	0
	.align	8
	.quad	.L109
	.word	16
	.word	0
	.align	8
	.quad	.L107
	.word	16
	.word	0
	.align	8
	.section .note.GNU-stack,"",%progbits
