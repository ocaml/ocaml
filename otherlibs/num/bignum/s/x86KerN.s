# Copyright     Digital Equipment Corporation & INRIA     1988, 1989
#
#		KerN for the 80386
#		[Bepaul]
#
		.data
		.type	copyright,@object
		.size	copyright,72
copyright:	.string	"@(#)KerN.c: copyright Digital Equipment Corporation & INRIA 1988, 1989\n"

		.text
		.align	16
		.globl	BnnSetToZero
BnnSetToZero:	movl	4(%esp),%edx		# nn
		movl	8(%esp),%eax		# nl
		testl	%eax,%eax
		je	BSTZ2			# if(nl==0) return
		.align		16
BSTZ1:		movl	$0,(%edx)		# *nn = 0
		decl	%eax			# nl--
		leal	4(%edx),%edx		# nn += 1
		jne	BSTZ1			# if(nl) BSTZ1
BSTZ2:		ret

		.align	16
		.globl	BnnAssign
BnnAssign:	pushl	%ebx
		movl	8(%esp),%edx		# mm
		movl	12(%esp),%ebx		# nn
		movl	16(%esp),%eax		# nl
		testl	%eax,%eax		# if(nl<=0) return
		je	BAG4
		sall	$2,%eax
		leal	(%eax,%ebx),%ecx	# nnlim=nn+nl
		cmpl	%ebx,%edx
		jb	BAG1			# if(mm<nn) BAG1
		cmpl	%ecx,%edx
		jbe	BAG2			# if(mm <= nnlim) BAG2
		.align		16
BAG1:		movl	(%ebx),%eax		# A = *nn
		leal	4(%ebx),%ebx		# nn += 1
		movl	%eax,(%edx)		# *mm = A
		leal	4(%edx),%edx		# mm += 1
		cmpl	%ecx,%ebx		# if(nn < nnlim) BAG1
		jb	BAG1
		popl	%ebx			# return
		ret
BAG2:		cmpl	%ebx,%edx
		jbe	BAG4			# if(mm <= nn) return
		addl	%eax,%edx		# mm += nl
		.align		16
BAG3:		addl	$-4,%edx		# mm--
		addl	$-4,%ecx		# nnlim--
		movl	(%ecx),%eax		# A = *nnlim
		movl	%eax,(%edx)		# *mm = A
		cmpl	%ecx,%ebx		# if(nn<nnlim) BAG3
		jb	BAG3
BAG4:		popl	%ebx
		ret

		.align	16
		.globl	BnnSetDigit
BnnSetDigit:	movl	4(%esp),%edx		# nn
		movl	8(%esp),%eax		# d
		movl	%eax,(%edx)		# *nn = d
		ret

		.align	16
		.globl	BnnGetDigit
BnnGetDigit:	movl	4(%esp),%eax		# nn
		movl	(%eax),%eax		# return(*nn)
		ret

		.align	16
		.globl	BnnNumDigits
BnnNumDigits:	movl	8(%esp),%eax		# nl
		leal	0(,%eax,4),%edx
		addl	4(%esp),%edx		# nn += nl
		jmp	BND2
		.align		16
BND1:		decl	%eax			# nl--
BND2:		testl	%eax,%eax		# if(nl == 0) BND3
		je	BND3
		addl	$-4,%edx		# nn--
		cmpl	$0,(%edx)		# if(nn-- != 0) BND1
		je	BND1
BND3:		testl	%eax,%eax		# if(nl != 0) return(nl)
		jne	BND4
		movl	$1,%eax			# return(1)
BND4:		ret

		.align	16
		.globl	BnnNumLeadingZeroBitsInDigit
BnnNumLeadingZeroBitsInDigit:
		movl	4(%esp),%edx		# d
		xorl	%eax,%eax		# p = 0
		testl	%edx,%edx		# if(d) BNLZ1
		jne	BNLZ1
		movl	$32,%eax		# return(32)
		ret
BNLZ1:		testl	$-65536,%edx		# if(d & 0xFFFF0000) BNLZ2
		jne	BNLZ2
		movl	$16,%eax		# p = 16
		sall	$16,%edx		# d <<= 16
BNLZ2:		testl	$-16777216,%edx		# if(d & 0xFF000000) BNLZ3
		jne	BNLZ3
		addl	$8,%eax			# p += 8
		sall	$8,%edx			# d <<= 8
BNLZ3:		testl	$-268435456,%edx	# if(d & 0xF0000000) BNLZ4
		jne	BNLZ4
		addl	$4,%eax			# p += 4
		sall	$4,%edx			# d <<= 4
BNLZ4:		testl	$-1073741824,%edx	# if(d & 0xC0000000) BNLZ5
		jne	BNLZ5
		addl	$2,%eax			# p += 2
		sall	$2,%edx			# d <<= 2
BNLZ5:		testl	%edx,%edx		# if(d) BNLZ6
		jl	BNLZ6
		incl	%eax			# p += 1
BNLZ6:		ret

		.align	16
		.globl	BnnDoesDigitFitInWord
BnnDoesDigitFitInWord:
		movl	$1,%eax
		ret

		.align	16
		.globl	BnnIsDigitZero
BnnIsDigitZero:	cmpl	$0,4(%esp)
		sete	%al
		andl	$255,%eax
		ret

		.align	16
		.globl BnnIsDigitNormalized
BnnIsDigitNormalized:
		movl	4(%esp),%eax
		shrl	$31,%eax
		ret

		.align	16
		.globl	BnnIsDigitOdd
BnnIsDigitOdd:	xorl	%eax,%eax
		testb	$1,4(%esp)
		setnz	%al
		ret

		.align	16
		.globl	BnnCompareDigits
BnnCompareDigits:
		movl	4(%esp),%ecx		# d1
		movl	8(%esp),%edx		# d2
		xorl	%eax,%eax
		cmpl	%edx,%ecx
		ja	.LBCD1
		je	.LBCD2
		movl	$-1,%eax
		ret
		.align	16
.LBCD1:		movl	$1,%eax
.LBCD2:		ret

#		movl	4(%esp),%eax		# d1
#		movl	8(%esp),%edx		# d2
#		subl	%edx,%eax		# d1 = d1 - d2
#		setnz	%cl			# cl = d1 == d2 ? 0 : 1
#		sarl	$31,%eax		# d2 = SIGN(d2) * -1
#		or	%cl,%al			# return(d2 | cl)
#		ret

#		cmpl	%ecx,%edx
#		setc	%al			# A = CF
#		setnz	%cl			# Z = !ZF
#		andl	$255,%eax
#		andl	$255,%ecx
#		subl	%ecx,%eax		# A = CF - !ZF
#		orl	%ecx,%eax		# A = (CF-!ZF)|!ZF
#		ret

		.align	16
		.globl	BnnComplement
BnnComplement:	movl	4(%esp),%edx		# nn
		movl	8(%esp),%eax		# nl
		testl	%eax,%eax		# if(nl==0) return
		je	BCOMP2
		leal	(%edx,%eax,4),%ecx	# nnlim = nn+nl
		.align		16
BCOMP1:		notl	(%edx)			# *nn = !*nn
		addl	$4,%edx			# nn++
		cmpl	%ecx,%edx		# if(nn<nnlim) BCOMP1
		jb	BCOMP1
BCOMP2:		ret

		.align	16
		.globl	BnnAndDigits
BnnAndDigits:
		movl	4(%esp),%eax
		movl	8(%esp),%edx
		andl	%edx,(%eax)
		ret

		.align	16
		.globl	BnnOrDigits
BnnOrDigits:	movl	4(%esp),%eax
		movl	8(%esp),%edx
		orl	%edx,(%eax)
		ret

		.align	16
		.globl	BnnXorDigits
BnnXorDigits:	movl	4(%esp),%eax
		movl	8(%esp),%edx
		xorl	%edx,(%eax)
		ret

		.align	16
		.globl	BnnShiftLeft
BnnShiftLeft:	pushl	%ebp
		pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	20(%esp),%ebp		# mm
		movl	24(%esp),%ebx		# ml
		movl	28(%esp),%ecx		# nbi
		xorl	%eax,%eax		# res = 0
		testl	%ecx,%ecx		# if(nbi == 0) return(res)
		je	BSL2
		testl	%ebx,%ebx		# if(ml == 0) return(res)
		je	BSL2
		movl	$32,%edx		# rnbi = 32
		subl	%ecx,%edx		# rnbi -= nbi
		bswap	%edx			# Same as rnbi << 24..
		orl	%edx,%ecx		# C = rnbi .. nbi
		.align		16
BSL1:
		movl	(%ebp),%esi		# save = *mm
		movl	(%ebp),%edi		# X = save
		sall	%cl,%edi		# X << nbi
		orl	%eax,%edi		# X |= res
		movl	%edi,(%ebp)		# *mm = X
		addl	$4,%ebp			# mm++
		movl	%esi,%eax		# res = save
		bswap	%ecx
		shrl	%cl,%eax		# res >>= rnbi
		bswap	%ecx
		decl	%ebx			# ml--
		jne	BSL1			# if(ml) BSL1
BSL2:		popl	%ebx
		popl	%esi
		popl	%edi
		popl	%ebp
		ret

		.align	16
		.globl	BnnShiftRight
BnnShiftRight:	pushl	%ebp
		pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	20(%esp),%ebp		# mm
		movl	24(%esp),%ebx		# ml
		movl	28(%esp),%ecx		# nbi
		xorl	%eax,%eax		# res = 0
		testl	%ecx,%ecx		# if(nbi == 0) return(res)
		je	BSR2
		testl	%ebx,%ebx		# if(ml == 0) return(res)
		je	BSR2
		leal	(%ebp,%ebx,4),%ebp	# mm += ml
		movl	$32,%edx		# rnbi = 32
		subl	%ecx,%edx		# rnbi -= nbi
		bswap	%edx			# Same as rnbi << 24..
		orl	%edx,%ecx		# C = rnbi .. nbi
		.align		16
BSR1:
		addl	$-4,%ebp		# mm--
		movl	(%ebp),%esi		# save = *mm
		movl	(%ebp),%edi		# X = save
		shrl	%cl,%edi		# X >>= nbi
		orl	%eax,%edi		# X |= res
		movl	%edi,(%ebp)		# *mm = X
		movl	%esi,%eax		# res = save
		bswap	%ecx
		sall	%cl,%eax		# res <<= rnbi
		bswap	%ecx
		decl	%ebx			# ml--
		jne	BSR1			# if(ml) BSR1
BSR2:		popl	%ebx
		popl	%esi
		popl	%edi
		popl	%ebp
		ret

		.align	16
		.globl	BnnAddCarry
BnnAddCarry:	movl	4(%esp),%edx		# nn
		movl	8(%esp),%ecx		# nl
		cmpl	$0,12(%esp)		# if(carryin==0) return(0);
		je	BAC4
BAC1:		testl	%ecx,%ecx		# if(nl==0) return(1);
		je	BAC3
		.align		16
BAC2:		movl	(%edx),%eax		# X = *nn
		addl	$1,%eax			# X++
		movl	%eax,(%edx)		# *nn = X
		jnc	BAC4			# if !CF return(0);
		leal	4(%edx),%edx		# nn += 1;
		decl	%ecx			# nl--
		jnz	BAC2			# if(nl!=0) BAC2
BAC3:		movl	$1,%eax			# return(1);
		ret
BAC4:		xorl	%eax,%eax		# return(0);
		ret

		.align	16
		.globl	BnnAdd
BnnAdd:		pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	16(%esp),%edx		# mm
		movl	20(%esp),%ecx		# ml
		movl	24(%esp),%ebx		# nn
		movl	28(%esp),%esi		# nl
		movl	32(%esp),%eax		# c
		subl	%esi,%ecx		# ml -= nl
		testl	%esi,%esi		# if(nl == 0) BADD2
		je	BADD2
		neg	%eax			# CF = c
		.align		16
BADD1:
		movl	(%ebx),%eax		# c = *nn
		movl	(%edx),%edi		# X = *mm
		adc	%eax,%edi		# X += c + CF
		movl	%edi,(%edx)		# *mm = X
		decl	%esi			# nl--
		leal	4(%ebx),%ebx		# nn += 1;
		leal	4(%edx),%edx		# mm += 1;
		jne	BADD1			# if(nl != 0) BADD1
		setc	%al			# c = CF
		andl	$255,%eax
BADD2:		testl	%eax,%eax		# if(c == 0) return(0);
		je	BADD5
		testl	%ecx,%ecx		# if(ml==0) return(1);
		je	BADD4
		.align		16
BADD3:		incl	(%edx)			# (*mm)++
		jnz	BADD5			# if !ZF return(0);
		addl	$4,%edx			# mm++
		decl	%ecx			# ml--
		jnz	BADD3			# if(ml!=0) BADD3
BADD4:		movl	$1,%eax			# return(1);
		popl	%ebx
		popl	%esi
		popl	%edi
		ret
BADD5:		xorl	%eax,%eax		# return(0);
		popl	%ebx
		popl	%esi
		popl	%edi
		ret

		.align	16
		.globl	BnnSubtractBorrow
BnnSubtractBorrow:
		movl	4(%esp),%edx		# nn
		movl	8(%esp),%ecx		# nl
		cmpl	$0,12(%esp)		# if(carryin==1) return(1);
		jne	BSB4
BSB1:		testl	%ecx,%ecx		# if(nl==0) return(0);
		je	BSB3
		.align		16
BSB2:		subl	$1,(%edx)		# (*nn)--
		jnc	BSB4			# if !CF return(1);
		addl	$4,%edx			# nn++
		decl	%ecx			# nl--
		jnz	BSB2			# if(nl!=0) BSB2
BSB3:		xorl	%eax,%eax		# return(0);
		ret
BSB4:		movl	$1,%eax			# return(1);
		ret

		.align	16
		.globl	BnnSubtract
BnnSubtract:	pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	16(%esp),%edx		# mm
		movl	20(%esp),%ecx		# ml
		movl	24(%esp),%ebx		# nn
		movl	28(%esp),%esi		# nl
		movl	32(%esp),%eax		# c
		subl	%esi,%ecx		# ml -= nl
		testl	%esi,%esi		# if(nl) BS2
		je	BS2
		xorl	$1,%eax			# c = !c;
		neg	%eax			# CF = c
		.align		16
BS1:		movl	(%edx),%edi		# X = *mm
		movl	(%ebx),%eax		# c = *nn
		sbb	%eax,%edi		# X -= c + CF
		movl	%edi,(%edx)		# *mm = X
		leal	4(%ebx),%ebx		# nn += 1;
		leal	4(%edx),%edx		# mm += 1;
		decl	%esi			# nl--
		jne	BS1			# if(nl != 0) BS1
		setc	%al			# c = CF
		andl	$255,%eax
		xorl	$1,%eax			# c = !c;
BS2:		testl	%eax,%eax		# if(c == 1) return(1);
		jne	BS5
		testl	%ecx,%ecx		# if(ml==0) return(0);
		je	BS4
		.align		16
BS3:		subl	$1,(%edx)		# (*mm)--
		jnc	BS5			# if !CF return(1);
		addl	$4,%edx			# mm++
		decl	%ecx			# ml--
		jnz	BS3			# if(ml!=0) BS3
BS4:		xorl	%eax,%eax		# return(0);
		popl	%ebx
		popl	%esi
		popl	%edi
		ret
BS5:		movl	$1,%eax			# return(1);
		popl	%ebx
		popl	%esi
		popl	%edi
		ret

		.align	16
		.globl	BnnMultiplyDigit
BnnMultiplyDigit:
		movl	20(%esp),%ecx		# d
		testl	%ecx,%ecx		# if(d!=0) BM1
		jne	BMD1			
		xorl	%eax,%eax		# return(0);
		ret
BMD1:		cmpl	$1,%ecx			# if(d!=1) BM2
		jne	BMD2
		movl	$0,20(%esp)
		jmp	BnnAdd			# return(BnnAdd(pp,pl,mm,ml,0)
BMD2:		pushl	%ebp
		pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	20(%esp),%edi		# pp
		movl	28(%esp),%esi		# mm
		movl	32(%esp),%ebp		# ml
		subl	%ebp,24(%esp)		# pl -= ml
		xorl	%ebx,%ebx		# low = 0
		testl	%ebp,%ebp
		je	BMD7			# if(ml == 0) return(0);
		.align		16
BMD3:		movl	(%esi),%eax		# XL = *mm
		addl	$4,%esi			# mm++
		mul	%ecx			# XH:XL = D*XL
		addl	%ebx,%eax		# XL += low
		adc	$0,%edx			# XH += CF
		addl	(%edi),%eax		# XL += *pp (reverse ?!)
		adc	$0,%edx			# XH += CF
		movl	%eax,(%edi)		# *pp = XL
		addl	$4,%edi			# pp++
		movl	%edx,%ebx		# low = XH
BMD4:		decl	%ebp			# ml--
		jne	BMD3			# if(ml) BMD3
		movl	24(%esp),%edx		# pl
		addl	%ebx,(%edi)		# *pp += low
		jnc	BMD7			# if !CF return(0)
		decl	%edx			# pl--
		je	BMD6			# if(pl == 0) return(1)
		addl	$4,%edi			# pp++
		.align		16
BMD5:		addl	$1,(%edi)		# (*pp)++
		jnc	BMD7			# if !CF return(0);
		addl	$4,%edi			# pp++
		decl	%edx			# pl--
		jnz	BMD5			# if(pl!=0) BMD5
BMD6:		movl	$1,%eax			# return(1);
		popl	%ebx
		popl	%esi
		popl	%edi
		popl	%ebp
		ret
BMD7:		xorl	%eax,%eax		# return(0);
		popl	%ebx
		popl	%esi
		popl	%edi
		popl	%ebp
		ret

		.align	16
		.globl	BnnDivideDigit
BnnDivideDigit:	pushl	%edi
		pushl	%esi
		pushl	%ebx
		movl	16(%esp),%edi		# qq
		movl	20(%esp),%esi		# nn
		movl	24(%esp),%ecx		# nl
		movl	28(%esp),%ebx		# d
		leal	(%esi,%ecx,4),%esi	# nn+=nl
		decl	%ecx			# nl--;
		leal	(%edi,%ecx,4),%edi	# qq += ql
		addl	$-4,%esi		# nn--
		movl	(%esi),%edx		# XH = *nn;
		testl	%ecx,%ecx
		je	BDD2			# if(nl==0) return(XH)
		.align		16
BDD1:		addl	$-4,%esi		# nn--
		movl	(%esi),%eax		# XL = *nn
		div	%ebx			# XL = XH:XL / d;
						# XH = XH:XL % d;
		addl	$-4,%edi		# qq--
		movl	%eax,(%edi)		# *qq = XL;
		decl	%ecx			# nl--
		jnz	BDD1			# if(nl!=0) BDD1
BDD2:		movl	%edx,%eax		# return(XH);
		popl	%ebx
		popl	%esi
		popl	%edi
		ret
