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
	.globl	camlCallback__data_begin
camlCallback__data_begin:
	.text
	.globl	camlCallback__code_begin
camlCallback__code_begin:
	.data
	.quad	768
	.globl	camlCallback
camlCallback:
	.data
	.quad	2045
camlCallback__4:
	.quad	0x3ff0000000000000
	.data
	.quad	2045
camlCallback__5:
	.quad	0
	.data
	.quad	2045
camlCallback__6:
	.quad	0x413e848000000000
	.data
	.quad	3068
camlCallback__7:
	.ascii	"callback.ml"
	.space	4
	.byte	4
	.data
	.quad	3840
camlCallback__8:
	.quad	camlCallback__7
	.quad	49
	.quad	5
	.text
	.align	16
	.globl	camlCallback__entry
camlCallback__entry:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L109:
	movq	camlCallback__5@GOTPCREL(%rip), %rax
	movq	$3, %rbx
	cmpq	$2000001, %rbx
	jg	.L107
	movq	%rbx, (%rsp)
.L108:
	movq	camlCallback__4@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rsp)
	movq	%rax, %rdi
	movq	8(%rsp), %rsi
	movq	add_stub@GOTPCREL(%rip), %rax
	call	caml_c_call@PLT
.L110:

	movq    caml_young_ptr@GOTPCREL(%rip), %r11
	movq    (%r11), %r15
end6:
	movq	8(%rsp), %rsi
	movq	%rax, %rdi
	movq	add_stub@GOTPCREL(%rip), %rax
	call	caml_c_call@PLT
.L111:
	movq	caml_young_ptr@GOTPCREL(%rip), %r11
	movq	(%r11), %r15
	movq	(%rsp), %rdi
	movq	%rdi, %rbx
	addq	$2, %rdi
	movq	%rdi, (%rsp)
	cmpq	$2000001, %rbx
	jne	.L108
.L107:
	movsd	(%rax), %xmm0
	movsd	.L112(%rip), %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L113
	je	.L106
.L113:
	call	caml_alloc2@PLT
.L114:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlCallback__8@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L106:
	movq	$1, %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.cfi_endproc
	.type camlCallback__entry,@function
	.size camlCallback__entry,. - camlCallback__entry
	.data
	.quad	add_stub
	.section .rodata.cst8,"a",@progbits
.L112:
	.quad	0x413e848000000000
	.text
	.globl	camlCallback__code_end
camlCallback__code_end:
	.data
				/* relocation table start */
	.align	8
				/* relocation table end */
	.data
	.globl	camlCallback__data_end
camlCallback__data_end:
	.long	0
	.globl	camlCallback__frametable
camlCallback__frametable:
	.quad	3
	.quad	.L114
	.word	32
	.word	0
	.align	8
	.quad	.L111
	.word	32
	.word	0
	.align	8
	.quad	.L110
	.word	32
	.word	1
	.word	8
	.align	8
	.section .note.GNU-stack,"",%progbits
