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
	.globl	camlOutput_params__data_begin
camlOutput_params__data_begin:
	.text
	.globl	camlOutput_params__code_begin
camlOutput_params__code_begin:
	.data
	.quad	6912
	.globl	camlOutput_params
camlOutput_params:
	.quad	1
	.quad	1
	.quad	1
	.quad	1
	.quad	1
	.quad	1
	.data
	.quad	4092
camlOutput_params__4:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__5:
	.quad	camlOutput_params__4
	.quad	23
	.quad	5
	.data
	.quad	4092
camlOutput_params__6:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__7:
	.quad	camlOutput_params__6
	.quad	29
	.quad	5
	.data
	.quad	4092
camlOutput_params__8:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__9:
	.quad	camlOutput_params__8
	.quad	35
	.quad	5
	.data
	.quad	2045
camlOutput_params__10:
	.quad	0x4018000000000000
	.data
	.quad	2045
camlOutput_params__11:
	.quad	0x3ff0000000000000
	.data
	.quad	2045
camlOutput_params__12:
	.quad	0x401c000000000000
	.data
	.quad	4092
camlOutput_params__13:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__14:
	.quad	camlOutput_params__13
	.quad	59
	.quad	5
	.data
	.quad	4092
camlOutput_params__15:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__16:
	.quad	camlOutput_params__15
	.quad	63
	.quad	5
	.data
	.quad	2045
camlOutput_params__17:
	.quad	0x4020000000000000
	.data
	.quad	4092
camlOutput_params__18:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__19:
	.quad	camlOutput_params__18
	.quad	67
	.quad	5
	.data
	.quad	3071
camlOutput_params__20:
	.quad	caml_int64_ops
	.quad	6
	.data
	.quad	3071
camlOutput_params__21:
	.quad	caml_int64_ops
	.quad	1
	.data
	.quad	3071
camlOutput_params__22:
	.quad	caml_int64_ops
	.quad	7
	.data
	.quad	4092
camlOutput_params__23:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__24:
	.quad	camlOutput_params__23
	.quad	91
	.quad	5
	.data
	.quad	4092
camlOutput_params__25:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__26:
	.quad	camlOutput_params__25
	.quad	95
	.quad	5
	.data
	.quad	3071
camlOutput_params__27:
	.quad	caml_int64_ops
	.quad	8
	.data
	.quad	4092
camlOutput_params__28:
	.ascii	"output_params.ml"
	.space	7
	.byte	7
	.data
	.quad	3840
camlOutput_params__29:
	.quad	camlOutput_params__28
	.quad	99
	.quad	5
	.text
	.align	16
	.globl	camlOutput_params__entry
camlOutput_params__entry:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L115:
	call	caml_alloc3@PLT
.L116:
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	$13, (%rax)
	movq	camlOutput_params@GOTPCREL(%rip), %rbx
	movq	%rax, 16(%rbx)
	addq	$16, %rax
	movq	$1024, -8(%rax)
	movq	16(%rbx), %rdi
	movq	%rdi, (%rax)
	movq	%rax, 24(%rbx)
	movq	$13, %rax
	add	$3, %rax	# add_int
	subq	$1, %rax
	cmpq	$15, %rax
	je	.L114
	call	caml_alloc2@PLT
.L117:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__5@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L114:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	(%rbx), %rdi
	add	$3, %rdi	# add_int
	subq	$1, %rdi
	movq	%rdi, (%rbx)
	movq	16(%rax), %rax
	movq	(%rax), %rax
	cmpq	$15, %rax
	je	.L113
	call	caml_alloc2@PLT
.L118:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__7@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L113:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	24(%rax), %rbx
	movq	-8(%rbx), %rdi
	cmpq	$1023, %rdi
	jbe	.L119
	movq	(%rbx), %rbx
	movq	(%rbx), %rdi
	add	$3, %rdi	# add_int
	subq	$1, %rdi
	movq	%rdi, (%rbx)
	movq	24(%rax), %rax
	movq	-8(%rax), %rbx
	cmpq	$1023, %rbx
	jbe	.L119
	movq	(%rax), %rax
	movq	(%rax), %rax
	cmpq	$17, %rax
	je	.L112
	call	caml_alloc2@PLT
.L120:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__9@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L112:
	call	caml_alloc3@PLT
.L121:
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	camlOutput_params__10@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params@GOTPCREL(%rip), %rbx
	movq	%rax, 32(%rbx)
	addq	$16, %rax
	movq	$1024, -8(%rax)
	movq	32(%rbx), %rdi
	movq	%rdi, (%rax)
	movq	%rax, 40(%rbx)
	movsd	.L122(%rip), %xmm0
	movsd	.L123(%rip), %xmm1
	addsd	%xmm1, %xmm0	# add_float
	movsd	.L124(%rip), %xmm1
	ucomisd	%xmm1, %xmm0
	jp	.L125
	je	.L111
.L125:
	call	caml_alloc2@PLT
.L126:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__14@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L111:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	32(%rax), %rbx
	movq	(%rbx), %rax
	movsd	(%rax), %xmm0
	movsd	.L123(%rip), %xmm1
	addsd	%xmm1, %xmm0	# add_float
	call	caml_alloc1@PLT
.L127:
	leaq	8(%r15), %rax
	movq	$1277, -8(%rax)
	movsd	%xmm0, (%rax)
	movq	%rax, (%rbx)
	movsd	.L124(%rip), %xmm0
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	32(%rax), %rax
	movq	(%rax), %rax
	movsd	(%rax), %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L128
	je	.L110
.L128:
	call	caml_alloc2@PLT
.L129:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__16@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L110:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	40(%rax), %rax
	movq	-8(%rax), %rbx
	cmpq	$1023, %rbx
	jbe	.L119
	movq	(%rax), %rbx
	movq	(%rbx), %rax
	movsd	(%rax), %xmm0
	movsd	.L123(%rip), %xmm1
	addsd	%xmm1, %xmm0	# add_float
	call	caml_alloc1@PLT
.L130:
	leaq	8(%r15), %rax
	movq	$1277, -8(%rax)
	movsd	%xmm0, (%rax)
	movq	%rax, (%rbx)
	movsd	.L131(%rip), %xmm0
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	40(%rax), %rax
	movq	-8(%rax), %rbx
	cmpq	$1023, %rbx
	jbe	.L119
	movq	(%rax), %rax
	movq	(%rax), %rax
	movsd	(%rax), %xmm1
	ucomisd	%xmm0, %xmm1
	jp	.L132
	je	.L109
.L132:
	call	caml_alloc2@PLT
.L133:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__19@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L109:
	call	caml_alloc3@PLT
.L134:
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	camlOutput_params__20@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	addq	$16, %rax
	movq	$1024, -8(%rax)
	movq	(%rbx), %rdi
	movq	%rdi, (%rax)
	movq	%rax, 8(%rbx)
	movq	$6, %rax
	add	$1, %rax	# add_int64
	movq	$7, %rbx
	cmpq	%rbx, %rax
	je	.L108
	call	caml_alloc2@PLT
.L135:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__24@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L108:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	(%rax), %rbx
	movq	(%rbx), %rax
	movq	8(%rax), %rdi
	add	$1, %rdi	# add_int64
	call	caml_alloc2@PLT
.L136:
	leaq	8(%r15), %rax
	movq	$2303, -8(%rax)
	movq	caml_int64_ops@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	%rdi, 8(%rax)
	movq	%rax, (%rbx)
	movq	$7, %rax
	movq	camlOutput_params@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rbx
	movq	(%rbx), %rbx
	movq	8(%rbx), %rbx
	cmpq	%rax, %rbx
	je	.L107
	call	caml_alloc2@PLT
.L137:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__26@GOTPCREL(%rip), %rbx
	movq	%rbx, 8(%rax)
	movq	%r14, %rsp
	popq	%r14
	ret
.L107:
	movq	camlOutput_params@GOTPCREL(%rip), %rax
	movq	8(%rax), %rax
	movq	-8(%rax), %rbx
	cmpq	$1023, %rbx
	jbe	.L119
	movq	(%rax), %rbx
	movq	(%rbx), %rax
	movq	8(%rax), %rdi
	add	$1, %rdi	# add_int64
	call	caml_alloc2@PLT
.L138:
	leaq	8(%r15), %rax
	movq	$2303, -8(%rax)
	movq	caml_int64_ops@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	%rdi, 8(%rax)
	movq	%rax, (%rbx)
	movq	$8, %rax
	movq	camlOutput_params@GOTPCREL(%rip), %rbx
	movq	8(%rbx), %rbx
	movq	-8(%rbx), %rdi
	cmpq	$1023, %rdi
	jbe	.L119
	movq	(%rbx), %rbx
	movq	(%rbx), %rbx
	movq	8(%rbx), %rbx
	cmpq	%rax, %rbx
	je	.L106
	call	caml_alloc2@PLT
.L139:
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	caml_exn_Assert_failure@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	camlOutput_params__29@GOTPCREL(%rip), %rbx
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
.L119:
	call	caml_ml_array_bound_error@PLT
	.cfi_endproc
	.type camlOutput_params__entry,@function
	.size camlOutput_params__entry,. - camlOutput_params__entry
	.data
	.section .rodata.cst8,"a",@progbits
.L131:
	.quad	0x4020000000000000
.L124:
	.quad	0x401c000000000000
.L123:
	.quad	0x3ff0000000000000
.L122:
	.quad	0x4018000000000000
	.text
	.globl	camlOutput_params__code_end
camlOutput_params__code_end:
	.data
				/* relocation table start */
	.align	8
				/* relocation table end */
	.data
	.globl	camlOutput_params__data_end
camlOutput_params__data_end:
	.long	0
	.globl	camlOutput_params__frametable
camlOutput_params__frametable:
	.quad	16
	.quad	.L139
	.word	16
	.word	0
	.align	8
	.quad	.L138
	.word	16
	.word	2
	.word	5
	.word	3
	.align	8
	.quad	.L137
	.word	16
	.word	0
	.align	8
	.quad	.L136
	.word	16
	.word	2
	.word	5
	.word	3
	.align	8
	.quad	.L135
	.word	16
	.word	0
	.align	8
	.quad	.L134
	.word	16
	.word	0
	.align	8
	.quad	.L133
	.word	16
	.word	0
	.align	8
	.quad	.L130
	.word	16
	.word	1
	.word	3
	.align	8
	.quad	.L129
	.word	16
	.word	0
	.align	8
	.quad	.L127
	.word	16
	.word	1
	.word	3
	.align	8
	.quad	.L126
	.word	16
	.word	0
	.align	8
	.quad	.L121
	.word	16
	.word	0
	.align	8
	.quad	.L120
	.word	16
	.word	0
	.align	8
	.quad	.L118
	.word	16
	.word	0
	.align	8
	.quad	.L117
	.word	16
	.word	0
	.align	8
	.quad	.L116
	.word	16
	.word	0
	.align	8
	.section .note.GNU-stack,"",%progbits
