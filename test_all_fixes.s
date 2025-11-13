	.file	""
	.data
	.globl	_camlTest_all_fixes$data_begin
_camlTest_all_fixes$data_begin:
	.text
	.globl	_camlTest_all_fixes$code_begin
_camlTest_all_fixes$code_begin:
	nop
	.align	3
	.data
	.align	3
	.data
	.align	3
	.quad	4087
	.globl	_camlTest_all_fixes$3
_camlTest_all_fixes$3:
	.quad	_caml_curry2
	.quad	144115188075855879
	.quad	_camlTest_all_fixes$add_274
	.data
	.align	3
	.quad	4087
	.globl	_camlTest_all_fixes$2
_camlTest_all_fixes$2:
	.quad	_caml_curry2
	.quad	144115188075855879
	.quad	_camlTest_all_fixes$multiply_278
	.data
	.align	3
	.quad	3063
	.globl	_camlTest_all_fixes$1
_camlTest_all_fixes$1:
	.quad	_camlTest_all_fixes$main_282
	.quad	72057594037927941
	.data
	.align	3
	.quad	3840
	.globl	_camlTest_all_fixes
	.globl	_camlTest_all_fixes
_camlTest_all_fixes:
	.quad	1
	.quad	1
	.quad	1
	.data
	.align	3
	.globl	_camlTest_all_fixes$gc_roots
	.globl	_camlTest_all_fixes$gc_roots
_camlTest_all_fixes$gc_roots:
	.quad	_camlTest_all_fixes
	.quad	0
	.text
	.align	3
	.globl	_camlTest_all_fixes$add_274
_camlTest_all_fixes$add_274:
	.file	1	"test_all_fixes.ml"
	.loc	1	1
	.cfi_startproc
L101:
L102:
	.loc	1	1
L103:
L100:
	.ifne (. - L102) - 0
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L104:
	.loc	1	1
	add	x2, x0, x1
	.ifne (. - L104) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L105:
	.loc	1	1
	sub	x0, x2, #1
	.ifne (. - L105) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L106:
	and	x30, x30, #0x00FFFFFFFFFFFFFF
	ret
	.ifne (. - L106) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.ifne (. - L101) - 16
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.cfi_endproc
_camlTest_all_fixes$add_274_end:
	.text
	.align	3
	.globl	_camlTest_all_fixes$multiply_278
_camlTest_all_fixes$multiply_278:
	.loc	1	3
	.cfi_startproc
L108:
L109:
L107:
	.ifne (. - L109) - 0
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L110:
	orr	x2, xzr, #1
	.ifne (. - L110) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L111:
	.loc	1	3
L112:
	asr	x3, x1, #1
	.ifne (. - L111) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L113:
	.loc	1	3
	sub	x4, x0, #1
	.ifne (. - L113) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L114:
	.loc	1	3
	madd	x0, x4, x3, x2
	.ifne (. - L114) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L115:
	and	x30, x30, #0x00FFFFFFFFFFFFFF
	ret
	.ifne (. - L115) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.ifne (. - L108) - 24
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.cfi_endproc
_camlTest_all_fixes$multiply_278_end:
	.text
	.align	3
	.globl	_camlTest_all_fixes$main_282
L117:
	mov	x16, #34
	stp	x16, x30, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, x30, [sp], #16
_camlTest_all_fixes$main_282:
	.loc	1	5
	.cfi_startproc
	ldr	x16, [x28, #40]
	add	x16, x16, #328
	cmp	sp, x16
	bcc	L117
L118:
L119:
	str	x30, [sp, #-8]
	.cfi_offset 30, -8
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.ifne (. - L119) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L120:
L116:
	.ifne (. - L120) - 0
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L121:
	movz	x0, #121, lsl #0
	.ifne (. - L121) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L122:
	.file	2	"stdlib.ml"
	.loc	2	489
L123:
	bl	_camlStdlib$string_of_int_175
L124:
	.ifne (. - L122) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L125:
	mov	x1, x0
	.ifne (. - L125) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L126:
	adrp	x3, _camlStdlib@GOTPAGE
	ldr	x3, [x3, _camlStdlib@GOTPAGEOFF]
	.ifne (. - L126) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L127:
	.loc	1	8
L128:
	ldr	x0, [x3, #304]
	.ifne (. - L127) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L129:
	.loc	2	489
L130:
	bl	_camlStdlib$output_string_253
L131:
	.ifne (. - L129) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L132:
	orr	x0, xzr, #1
	.ifne (. - L132) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L133:
	.loc	1	9
L134:
	add	sp, sp, #16
	.cfi_adjust_cfa_offset	-16
	ldr	x30, [sp, #-8]
	b	_camlStdlib$print_newline_372
	.cfi_adjust_cfa_offset	16
	.ifne (. - L133) - 12
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.ifne (. - L118) - 52
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.cfi_endproc
_camlTest_all_fixes$main_282_end:
	.text
	.align	3
	.globl	_camlTest_all_fixes$entry
_camlTest_all_fixes$entry:
	.cfi_startproc
L136:
L137:
	str	x30, [sp, #-8]
	.cfi_offset 30, -8
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset	16
	.ifne (. - L137) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L138:
L135:
	.ifne (. - L138) - 0
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L139:
	adrp	x1, _camlTest_all_fixes$3@GOTPAGE
	ldr	x1, [x1, _camlTest_all_fixes$3@GOTPAGEOFF]
	.ifne (. - L139) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L140:
	adrp	x0, _camlTest_all_fixes@GOTPAGE
	ldr	x0, [x0, _camlTest_all_fixes@GOTPAGEOFF]
	.ifne (. - L140) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L141:
	mov	x19, sp
	.cfi_remember_state
	.cfi_def_cfa_register 19
	ldr	x16, [x28, 64]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x19
	.cfi_restore_state
	.ifne (. - L141) - 20
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L142:
	adrp	x1, _camlTest_all_fixes$2@GOTPAGE
	ldr	x1, [x1, _camlTest_all_fixes$2@GOTPAGEOFF]
	.ifne (. - L142) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L143:
	adrp	x3, _camlTest_all_fixes@GOTPAGE
	ldr	x3, [x3, _camlTest_all_fixes@GOTPAGEOFF]
	.ifne (. - L143) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L144:
	add	x0, x3, #8
	.ifne (. - L144) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L145:
	mov	x19, sp
	.cfi_remember_state
	.cfi_def_cfa_register 19
	ldr	x16, [x28, 64]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x19
	.cfi_restore_state
	.ifne (. - L145) - 20
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L146:
	adrp	x1, _camlTest_all_fixes$1@GOTPAGE
	ldr	x1, [x1, _camlTest_all_fixes$1@GOTPAGEOFF]
	.ifne (. - L146) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L147:
	adrp	x6, _camlTest_all_fixes@GOTPAGE
	ldr	x6, [x6, _camlTest_all_fixes@GOTPAGEOFF]
	.ifne (. - L147) - 8
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L148:
	add	x0, x6, #16
	.ifne (. - L148) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L149:
	mov	x19, sp
	.cfi_remember_state
	.cfi_def_cfa_register 19
	ldr	x16, [x28, 64]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x19
	.cfi_restore_state
	.ifne (. - L149) - 20
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L150:
	orr	x0, xzr, #1
	.ifne (. - L150) - 4
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L151:
	.ifne (. - L151) - 0
	.error "Emit.instr_size: instruction length mismatch"
	.endif
L152:
	add	sp, sp, #16
	.cfi_adjust_cfa_offset	-16
	ldr	x30, [sp, #-8]
	and	x30, x30, #0x00FFFFFFFFFFFFFF
	ret
	.cfi_adjust_cfa_offset	16
	.ifne (. - L152) - 16
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.ifne (. - L136) - 144
	.error "Emit.instr_size: instruction length mismatch"
	.endif
	.cfi_endproc
_camlTest_all_fixes$entry_end:
	.data
	.align	3
	.text
	.globl	_camlTest_all_fixes$code_end
_camlTest_all_fixes$code_end:
	.data
	.quad	0
	.globl	_camlTest_all_fixes$data_end
_camlTest_all_fixes$data_end:
	.quad	0
	.align	3
	.globl	_camlTest_all_fixes$frametable
_camlTest_all_fixes$frametable:
	.quad	2
	.quad	L131
	.short	17
	.short	0
	.align	2
	.long	L153 - . + 0x0
	.align	3
	.quad	L124
	.short	17
	.short	0
	.align	2
	.long	L154 - . + 0x0
	.align	3
	.align	2
L154:
	.long	L156 - . + 0x1
	.long	0xf489dc0
	.long	L158 - . + 0x0
	.long	0x400898
	.align	2
L153:
	.long	L156 - . + 0x1
	.long	0xf4849c0
	.long	L158 - . + 0x0
	.long	0x400898
L157:
	.asciz	"test_all_fixes.ml"
L155:
	.asciz	"stdlib.ml"
	.align	2
L156:
	.long	L155 - . + 0x0
	.asciz	"Stdlib.print_int"
	.align	2
L158:
	.long	L157 - . + 0x0
	.asciz	"Test_all_fixes.main"
	.align	3

	# DWARF debugging information
	.section __DWARF,__debug_info,regular,debug
	.byte 0xb2,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x08,0x01
	.long 59
	.long 29
	.long 0
	.byte 0x23,0x06
	.long 193
	.byte 0x08,0x01,0x06
	.long 189
	.byte 0x08,0x05,0x03
	.long 77
	.quad _camlTest_all_fixes$add_274
	.quad _camlTest_all_fixes$add_274_end
	.byte 0x05
	.long 57
	.byte 0x19,0x00,0x00,0x00,0x01,0x50,0x05
	.long 57
	.byte 0x19,0x00,0x00,0x00,0x01,0x51,0x00,0x03
	.long 157
	.quad _camlTest_all_fixes$multiply_278
	.quad _camlTest_all_fixes$multiply_278_end
	.byte 0x05
	.long 57
	.byte 0x19,0x00,0x00,0x00,0x01,0x50,0x05
	.long 57
	.byte 0x19,0x00,0x00,0x00,0x01,0x51,0x00,0x03
	.long 129
	.quad _camlTest_all_fixes$main_282
	.quad _camlTest_all_fixes$main_282_end
	.byte 0x05
	.long 57
	.byte 0x19,0x00,0x00,0x00,0x01,0x50,0x00,0x02
	.long 104
	.quad _camlTest_all_fixes$entry
	.quad _camlTest_all_fixes$entry_end
	.byte 0x00
	.section __DWARF,__debug_abbrev,regular,debug
	.byte 0x01,0x11,0x01,0x03,0x0e,0x25,0x0e,0x1b,0x0e,0x13,0x0b,0x00,0x00,0x02,0x2e,0x00
	.byte 0x03,0x0e,0x11,0x01,0x12,0x01,0x3f,0x19,0x00,0x00,0x03,0x2e,0x01,0x03,0x0e,0x11
	.byte 0x01,0x12,0x01,0x3f,0x19,0x00,0x00,0x04,0x05,0x00,0x03,0x0e,0x02,0x18,0x00,0x00
	.byte 0x05,0x05,0x00,0x03,0x0e,0x49,0x13,0x02,0x18,0x00,0x00,0x06,0x24,0x00,0x03,0x0e
	.byte 0x0b,0x0b,0x3e,0x0b,0x00,0x00,0x07,0x0f,0x00,0x0b,0x0b,0x49,0x13,0x00,0x00,0x08
	.byte 0x2e,0x01,0x03,0x0e,0x49,0x13,0x11,0x01,0x12,0x01,0x3f,0x19,0x00,0x00,0x00
	.section __DWARF,__debug_str,regular,debug
Ldebug_str_start:
Lstr_0:
	.byte 0x2f,0x55,0x73,0x65,0x72,0x73,0x2f,0x6a,0x6f,0x65,0x6c,0x2f,0x57,0x6f,0x72,0x6b
	.byte 0x2f,0x6f,0x63,0x61,0x6d,0x6c,0x2f,0x64,0x77,0x61,0x72,0x66,0x00
Lstr_1:
	.byte 0x4f,0x43,0x61,0x6d,0x6c,0x20,0x35,0x2e,0x35,0x2e,0x30,0x2b,0x64,0x65,0x76,0x30
	.byte 0x2d,0x32,0x30,0x32,0x35,0x2d,0x30,0x34,0x2d,0x32,0x38,0x00
Lstr_2:
	.byte 0x52,0x00
Lstr_3:
	.byte 0x54,0x65,0x73,0x74,0x5f,0x61,0x6c,0x6c,0x5f,0x66,0x69,0x78,0x65,0x73,0x2e,0x6d
	.byte 0x6c,0x00
Lstr_4:
	.byte 0x63,0x61,0x6d,0x6c,0x54,0x65,0x73,0x74,0x5f,0x61,0x6c,0x6c,0x5f,0x66,0x69,0x78
	.byte 0x65,0x73,0x24,0x61,0x64,0x64,0x5f,0x32,0x37,0x34,0x00
Lstr_5:
	.byte 0x63,0x61,0x6d,0x6c,0x54,0x65,0x73,0x74,0x5f,0x61,0x6c,0x6c,0x5f,0x66,0x69,0x78
	.byte 0x65,0x73,0x24,0x65,0x6e,0x74,0x72,0x79,0x00
Lstr_6:
	.byte 0x63,0x61,0x6d,0x6c,0x54,0x65,0x73,0x74,0x5f,0x61,0x6c,0x6c,0x5f,0x66,0x69,0x78
	.byte 0x65,0x73,0x24,0x6d,0x61,0x69,0x6e,0x5f,0x32,0x38,0x32,0x00
Lstr_7:
	.byte 0x63,0x61,0x6d,0x6c,0x54,0x65,0x73,0x74,0x5f,0x61,0x6c,0x6c,0x5f,0x66,0x69,0x78
	.byte 0x65,0x73,0x24,0x6d,0x75,0x6c,0x74,0x69,0x70,0x6c,0x79,0x5f,0x32,0x37,0x38,0x00
Lstr_8:
	.byte 0x69,0x6e,0x74,0x00
Lstr_9:
	.byte 0x76,0x61,0x6c,0x75,0x65,0x00
	.section __DWARF,__debug_line,regular,debug
	.byte 0xc8,0x00,0x00,0x00,0x04,0x00,0x53,0x00,0x00,0x00,0x01,0x01,0x01,0xfb,0x0e,0x0d
	.byte 0x00,0x01,0x01,0x01,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x01,0x2f,0x55,0x73,0x65
	.byte 0x72,0x73,0x2f,0x6a,0x6f,0x65,0x6c,0x2f,0x57,0x6f,0x72,0x6b,0x2f,0x6f,0x63,0x61
	.byte 0x6d,0x6c,0x2f,0x64,0x77,0x61,0x72,0x66,0x00,0x00,0x74,0x65,0x73,0x74,0x5f,0x61
	.byte 0x6c,0x6c,0x5f,0x66,0x69,0x78,0x65,0x73,0x2e,0x6d,0x6c,0x00,0x00,0x00,0x00,0x73
	.byte 0x74,0x64,0x6c,0x69,0x62,0x2e,0x6d,0x6c,0x00,0x00,0x00,0x00,0x00,0x00,0x09,0x02
	.quad L103
	.byte 0x04,0x01,0x05,0x0e,0x01,0x00,0x09,0x02
	.quad L112
	.byte 0x05,0x13,0x03,0x02,0x01,0x00,0x09,0x02
	.quad L123
	.byte 0x04,0x02,0x05,0x27,0x03,0xe6,0x03,0x01,0x00,0x09,0x02
	.quad L128
	.byte 0x04,0x01,0x05,0x02,0x03,0x9f,0x7c,0x01,0x00,0x09,0x02
	.quad L130
	.byte 0x04,0x02,0x05,0x12,0x03,0xe1,0x03,0x01,0x00,0x09,0x02
	.quad L134
	.byte 0x04,0x01,0x05,0x02,0x03,0xa0,0x7c,0x01,0x00,0x01,0x01

