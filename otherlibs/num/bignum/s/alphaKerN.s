	.ugen	
	.verstamp	3 11
	.data	
	.align	3
	.align	0
$$4:
	.ascii	"@(#)KerN.c: copyright Digital Equipment Corporation & INRIA 1988, 1989\X0A\X00"
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnSetToZero
	.loc	2 63
 #   63	{
	.ent	BnnSetToZero 2
BnnSetToZero:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 63

	.loc	2 65
 #   64	    BigNum nnlim;
 #   65	    if (nl <= 0)
	beq	$17, $33
	.loc	2 66
 #   66		return;
	.loc	2 67
 #   67	    nnlim = nn+nl-1;
	s8addq	$17, $16, $0
	addq	$0, -8, $0
	.loc	2 68
 #   68	    do *nn = 0; while(nn++ < nnlim);
$32:
	.loc	2 68

	stq	$31, 0($16)
	cmpult	$16, $0, $17
	addq	$16, 8, $16
	bne	$17, $32
	.loc	2 69
 #   69	}
$33:
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnSetToZero
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnAssign
	.loc	2 80
 #   80	{
	.ent	BnnAssign 2
BnnAssign:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	bis	$16, $16, $1
	.loc	2 80

	.loc	2 82
 #   81	    BigNum nnlim;
 #   82	    if (nl <= 0)
	beq	$18, $37
	.loc	2 83
 #   83		return;
	.loc	2 84
 #   84	    nnlim = nn+nl;
	sll	$18, 3, $16
	addq	$16, $17, $19
	bis	$19, $19, $0
	.loc	2 88
 #   85	#ifdef MSDOS
 #   86	    if (realaddr(mm) < realaddr(nn) || realaddr(mm) > realaddr(nnlim))
 #   87	#else
 #   88	    if ((mm < nn) || ( mm > nnlim))
	cmpult	$1, $17, $2
	bne	$2, $34
	cmpult	$19, $1, $3
	beq	$3, $35
	.loc	2 90
 #   89	#endif
 #   90		do *mm++ = *nn++; while(nn < nnlim);
$34:
	.loc	2 90

	ldq	$4, 0($17)
	stq	$4, 0($1)
	addq	$1, 8, $1
	addq	$17, 8, $17
	cmpult	$17, $0, $5
	bne	$5, $34
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
$35:
	.loc	2 95
 #   91	    else
 #   92	#ifdef MSDOS
 #   93	    if (realaddr(mm) > realaddr(nn))
 #   94	#else
 #   95	    if (mm > nn)
	cmpult	$17, $1, $6
	beq	$6, $37
	.loc	2 97
 #   96	#endif
 #   97	    {
	.loc	2 98
 #   98		mm += nl;
	addq	$1, $16, $1
	.loc	2 99
 #   99		do *--mm = *--nnlim; while(nn < nnlim);
$36:
	.loc	2 99

	addq	$1, -8, $18
	bis	$18, $18, $16
	bis	$18, $18, $1
	addq	$0, -8, $0
	ldq	$7, 0($0)
	stq	$7, 0($16)
	cmpult	$17, $0, $8
	bne	$8, $36
	.loc	2 101
 #  100	    }
 #  101	}
$37:
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnAssign
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnSetDigit
	.loc	2 113
 #  113	{
	.ent	BnnSetDigit 2
BnnSetDigit:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 113

	.loc	2 114
 #  114	    *nn = d;
	stq	$17, 0($16)
	.loc	2 115
 #  115	}
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnSetDigit
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnGetDigit
	.loc	2 126
 #  126	{
	.ent	BnnGetDigit 2
BnnGetDigit:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 126

	.loc	2 127
 #  127	    return (*nn);
	ldq	$0, 0($16)
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnGetDigit
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnNumDigits
	.loc	2 140
 #  140	{
	.ent	BnnNumDigits 2
BnnNumDigits:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 140

	.loc	2 141
 #  141	    nn += nl;
	s8addq	$17, $16, $16
	.loc	2 143
 #  142	
 #  143	    while (nl != 0 && *--nn == 0)
	beq	$17, $39
	addq	$16, -8, $16
	ldq	$1, 0($16)
	bne	$1, $39
$38:
	.loc	2 144
 #  144	        nl--;
	addq	$17, -1, $17
	beq	$17, $39
	addq	$16, -8, $16
	ldq	$2, 0($16)
	beq	$2, $38
$39:
	.loc	2 146
 #  145	
 #  146	    return (nl == 0 ? 1 : nl);
	bis	$17, $17, $16
	cmoveq	$17, 1, $16
	bis	$16, $16, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnNumDigits
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnNumLeadingZeroBitsInDigit
	.loc	2 158
 #  158	{
	.ent	BnnNumLeadingZeroBitsInDigit 2
BnnNumLeadingZeroBitsInDigit:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	bis	$16, $16, $1
	.loc	2 158

	.loc	2 159
 #  159	    register int 	p = 0;
	bis	$31, $31, $17
	.loc	2 160
 #  160	    if (BN_DIGIT_SIZE == 16 || BN_DIGIT_SIZE == 32 || BN_DIGIT_SIZE == 64)
	.loc	2 161
 #  161	    {
	.loc	2 162
 #  162		register BigNumDigit mask = (~(BigNumDigit)0) << (BN_DIGIT_SIZE/2);
	ldiq	$0, -4294967296
	.loc	2 163
 #  163		register BigNumLength maskl = BN_DIGIT_SIZE/2;
	ldiq	$16, 32
	.loc	2 165
 #  164	
 #  165		if (d == 0) 
	bne	$1, $40
	.loc	2 166
 #  166		    return (BN_DIGIT_SIZE);
	ldiq	$0, 64
	.livereg	0x807F0002,0x3FC00000
	ret	$31, ($26), 1
$40:
	.loc	2 168
 #  167		while (maskl)
 #  168		{
	.loc	2 169
 #  169		    if ((d & mask) == 0)
	and	$1, $0, $2
	bne	$2, $41
	.loc	2 170
 #  170		    {
	.loc	2 171
 #  171			p += maskl;
	addq	$17, $16, $17
	addl	$17, 0, $17
	.loc	2 172
 #  172			d <<= maskl;
	sll	$1, $16, $1
$41:
	.loc	2 174
 #  173		    }
 #  174		    maskl >>= 1;
	srl	$16, 1, $16
	.loc	2 175
 #  175		    mask <<= maskl;
	sll	$0, $16, $0
	bne	$16, $40
	.loc	2 189
 #  189	    return (p);
	bis	$17, $17, $0
$42:
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnNumLeadingZeroBitsInDigit
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnDoesDigitFitInWord
	.loc	2 203
 #  203	{
	.ent	BnnDoesDigitFitInWord 2
BnnDoesDigitFitInWord:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 203

	.loc	2 205
 #  204	    /* The C compiler must evaluate the predicate at compile time */
 #  205	    if (BN_DIGIT_SIZE > BN_WORD_SIZE)
	.loc	2 206
 #  206	        return (d >= ((BigNumDigit)1) << BN_WORD_SIZE ? FALSE : TRUE);
	cmpult	$16, 4294967296, $17
	bis	$17, $17, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnDoesDigitFitInWord
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnIsDigitZero
	.loc	2 218
 #  218	{
	.ent	BnnIsDigitZero 2
BnnIsDigitZero:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 218

	.loc	2 219
 #  219	    return (d == 0);
	cmpeq	$16, 0, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnIsDigitZero
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnIsDigitNormalized
	.loc	2 232
 #  232	{
	.ent	BnnIsDigitNormalized 2
BnnIsDigitNormalized:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 232

	.loc	2 233
 #  233	    return (d & (((BigNumDigit)1) << (BN_DIGIT_SIZE - 1)) ? TRUE : FALSE);
	ldil	$17, 1
	cmovge	$16, 0, $17
	bis	$17, $17, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnIsDigitNormalized
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnIsDigitOdd
	.loc	2 245
 #  245	{
	.ent	BnnIsDigitOdd 2
BnnIsDigitOdd:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 245

	.loc	2 246
 #  246	    return (d & 1 ? TRUE : FALSE);
	ldil	$17, 1
	cmovlbc	$16, 0, $17
	bis	$17, $17, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnIsDigitOdd
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnCompareDigits
	.loc	2 260
 #  260	{
	.ent	BnnCompareDigits 2
BnnCompareDigits:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 260

	.loc	2 261
 #  261	    return (d1 > d2 ? BN_GT : (d1 == d2 ? BN_EQ : BN_LT));
	cmpult	$17, $16, $1
	beq	$1, $43
	ldil	$16, 1
	br	$31, $44
$43:
	subq	$16, $17, $2
	ldil	$0, -1
	cmoveq	$2, 0, $0
	bis	$0, $0, $16
$44:
	bis	$16, $16, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnCompareDigits
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnComplement
	.loc	2 273
 #  273	{
	.ent	BnnComplement 2
BnnComplement:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 273

	.loc	2 276
 #  274	    BigNum nnlim;
 #  275	
 #  276	    if (nl <= 0)
	beq	$17, $46
	.loc	2 277
 #  277		return;
	.loc	2 278
 #  278	    nnlim = nn+nl;
	s8addq	$17, $16, $0
	.loc	2 279
 #  279	    do
$45:
	.loc	2 280
 #  280	    {
	.loc	2 281
 #  281		nn++;
	addq	$16, 8, $16
	.loc	2 282
 #  282		nn[-1] = ~nn[-1];
	ldq	$1, -8($16)
	ornot	$31, $1, $2
	stq	$2, -8($16)
	cmpult	$16, $0, $3
	bne	$3, $45
	.loc	2 285
 #  283	    }
 #  284	    while (nn < nnlim);
 #  285	}
$46:
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnComplement
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnAndDigits
	.loc	2 297
 #  297	{
	.ent	BnnAndDigits 2
BnnAndDigits:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 297

	.loc	2 298
 #  298	    *n &= d;
	ldq	$1, 0($16)
	and	$1, $17, $2
	stq	$2, 0($16)
	.loc	2 299
 #  299	}
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnAndDigits
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnOrDigits
	.loc	2 310
 #  310	{
	.ent	BnnOrDigits 2
BnnOrDigits:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 310

	.loc	2 311
 #  311	    *n |= d;
	ldq	$1, 0($16)
	or	$1, $17, $2
	stq	$2, 0($16)
	.loc	2 312
 #  312	}
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnOrDigits
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnXorDigits
	.loc	2 323
 #  323	{
	.ent	BnnXorDigits 2
BnnXorDigits:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 323

	.loc	2 324
 #  324	    *n ^= d;
	ldq	$1, 0($16)
	xor	$1, $17, $2
	stq	$2, 0($16)
	.loc	2 325
 #  325	}
	.livereg	0x007F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnXorDigits
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnShiftLeft
	.loc	2 341
 #  341	{
	.ent	BnnShiftLeft 2
BnnShiftLeft:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	bis	$16, $16, $1
	addl	$18, 0, $18
	.loc	2 341

	.loc	2 342
 #  342	    register BigNumDigit res = 0, save;
	bis	$31, $31, $19
	.loc	2 346
 #  343	    	     int 	 rnbits;
 #  344	
 #  345	
 #  346	    if (nbits != 0)
	beq	$18, $48
	ldiq	$21, 1
	.loc	2 347
 #  347	    {
	.loc	2 348
 #  348		rnbits = BN_DIGIT_SIZE - nbits;
	.loc	2 350
 #  349	
 #  350		while (ml-- > 0) 
	cmpule	$21, $17, $16
	addq	$17, -1, $17
	beq	$16, $48
	bis	$18, $18, $0
	ldiq	$2, 64
	subq	$2, $0, $20
	addl	$20, 0, $20
$47:
	.loc	2 351
 #  351		{
	.loc	2 352
 #  352		    save = *mm;
	ldq	$18, 0($1)
	.loc	2 353
 #  353		    *mm++ = (save << nbits) | res;
	sll	$18, $0, $3
	or	$3, $19, $4
	stq	$4, 0($1)
	addq	$1, 8, $1
	.loc	2 354
 #  354		    res = save >> rnbits;
	srl	$18, $20, $19
	cmpule	$21, $17, $16
	addq	$17, -1, $17
	bne	$16, $47
$48:
	.loc	2 358
 #  355		}
 #  356	    }
 #  357	
 #  358	    return (res);
	bis	$19, $19, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnShiftLeft
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnShiftRight
	.loc	2 373
 #  373	{
	.ent	BnnShiftRight 2
BnnShiftRight:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	addl	$18, 0, $18
	.loc	2 373

	.loc	2 374
 #  374	    register BigNumDigit res = 0, save;
	bis	$31, $31, $19
	.loc	2 378
 #  375	    	     int 	 lnbits;
 #  376	
 #  377	
 #  378	    if (nbits != 0)
	beq	$18, $50
	ldiq	$1, 1
	.loc	2 379
 #  379	    {
	.loc	2 380
 #  380		mm += ml;
	s8addq	$17, $16, $16
	.loc	2 381
 #  381		lnbits = BN_DIGIT_SIZE - nbits;
	.loc	2 383
 #  382	
 #  383		while (ml-- > 0)
	cmpule	$1, $17, $20
	addq	$17, -1, $17
	beq	$20, $50
	bis	$18, $18, $0
	ldiq	$2, 64
	subq	$2, $0, $21
	addl	$21, 0, $21
$49:
	.loc	2 384
 #  384		{
	.loc	2 385
 #  385		    save = *(--mm);
	addq	$16, -8, $16
	ldq	$18, 0($16)
	.loc	2 386
 #  386		    *mm = (save >> nbits) | res;
	srl	$18, $0, $3
	or	$3, $19, $4
	stq	$4, 0($16)
	.loc	2 387
 #  387		    res = save << lnbits;
	sll	$18, $21, $19
	cmpule	$1, $17, $20
	addq	$17, -1, $17
	bne	$20, $49
$50:
	.loc	2 391
 #  388		}
 #  389	    }
 #  390	
 #  391	    return (res);
	bis	$19, $19, $0
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnShiftRight
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnAddCarry
	.loc	2 408
 #  408	{
	.ent	BnnAddCarry 2
BnnAddCarry:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	.loc	2 408

	.loc	2 409
 #  409	    if (carryin == 0) 
	bne	$18, $51
	.loc	2 410
 #  410	        return (0);
	bis	$31, $31, $0
	.livereg	0x807F0002,0x3FC00000
	ret	$31, ($26), 1
$51:
	.loc	2 412
 #  411	
 #  412	    if (nl == 0) 
	bne	$17, $52
	.loc	2 413
 #  413	        return (1);
	ldiq	$0, 1
	.livereg	0x807F0002,0x3FC00000
	ret	$31, ($26), 1
$52:
	ldiq	$19, 1
	.loc	2 415
 #  414	
 #  415	    while (nl > 0 && !(++(*nn++)))
	cmpule	$19, $17, $0
	beq	$0, $54
	ldq	$1, 0($16)
	addq	$1, 1, $2
	stq	$2, 0($16)
	ldq	$18, 0($16)
	cmpeq	$18, 0, $18
	addq	$16, 8, $16
	beq	$18, $54
$53:
	.loc	2 416
 #  416	        nl--;
	addq	$17, -1, $17
	cmpule	$19, $17, $0
	beq	$0, $54
	ldq	$3, 0($16)
	addq	$3, 1, $4
	stq	$4, 0($16)
	ldq	$18, 0($16)
	cmpeq	$18, 0, $18
	addq	$16, 8, $16
	bne	$18, $53
$54:
	.loc	2 418
 #  417	
 #  418	    return (nl > 0 ? 0 : 1);
	bis	$31, $31, $18
	cmoveq	$0, 1, $18
	bis	$18, $18, $0
$55:
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnAddCarry
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnAdd
	.loc	2 433
 #  433	{
	.ent	BnnAdd 2
BnnAdd:
	.option	O2
	ldgp	$gp, 0($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)
	.mask	0x04000000, -16
	.frame	$sp, 16, $26, 0
	.prologue	1
	bis	$16, $16, $1
	bis	$17, $17, $2
	.loc	2 433

	.loc	2 434
 #  434	    register BigNumProduct c = carryin;
	bis	$20, $20, $21
	.loc	2 437
 #  435	
 #  436	
 #  437	    ml -= nl;
	subq	$2, $19, $2
	.loc	2 439
 #  438	    /* test computed at compile time */
 #  439	    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
	ldiq	$17, 1
	.loc	2 450
 #  450	    {
	.loc	2 453
 #  451		register BigNumProduct save;
 #  452	
 #  453		while (nl > 0)
	cmpult	$19, $17, $3
	bne	$3, $59
$56:
	.loc	2 454
 #  454		{
	.loc	2 455
 #  455		    save = *mm;
	ldq	$0, 0($1)
	.loc	2 456
 #  456		    c += save;
	addq	$21, $0, $21
	.loc	2 457
 #  457		    if (c < save) 
	cmpult	$21, $0, $4
	beq	$4, $57
	.loc	2 458
 #  458		    {
	.loc	2 459
 #  459			*(mm++) = *(nn++);
	ldq	$5, 0($18)
	stq	$5, 0($1)
	addq	$1, 8, $1
	addq	$18, 8, $18
	.loc	2 460
 #  460			c = 1;
	bis	$17, $17, $21
	br	$31, $58
$57:
	.loc	2 463
 #  461		    }
 #  462		    else
 #  463		    {
	.loc	2 464
 #  464			save = *(nn++);
	ldq	$0, 0($18)
	addq	$18, 8, $18
	.loc	2 465
 #  465			c += save;
	addq	$21, $0, $21
	.loc	2 466
 #  466			*(mm++) = c;
	stq	$21, 0($1)
	addq	$1, 8, $1
	.loc	2 467
 #  467			c = (c < save) ? 1 : 0;
	cmpult	$21, $0, $16
	ldiq	$21, 1
	cmoveq	$16, 0, $21
$58:
	.loc	2 469
 #  468		    }
 #  469		    nl--;
	addq	$19, -1, $19
	cmpult	$19, $17, $6
	beq	$6, $56
$59:
	.loc	2 473
 #  470		}
 #  471	    }
 #  472	
 #  473	    return (BnnAddCarry (mm, ml, (BigNumCarry) c));
	bis	$1, $1, $16
	bis	$2, $2, $17
	bis	$21, $21, $18
	.livereg	0x0001F002,0x00000000
	jsr	$26, BnnAddCarry
	ldgp	$gp, 0($26)
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end	BnnAdd
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnSubtractBorrow
	.loc	2 490
 #  490	{
	.ent	BnnSubtractBorrow 2
BnnSubtractBorrow:
	.option	O2
	ldgp	$gp, 0($27)
	.frame	$sp, 0, $26, 0
	.prologue	1
	bis	$16, $16, $1
	.loc	2 490

	.loc	2 491
 #  491	    if (carryin == 1)
	subq	$18, 1, $2
	bne	$2, $60
	.loc	2 492
 #  492	        return (1);
	ldiq	$0, 1
	.livereg	0x807F0002,0x3FC00000
	ret	$31, ($26), 1
$60:
	.loc	2 493
 #  493	    if (nl == 0)
	bne	$17, $61
	.loc	2 494
 #  494	        return (0);
	bis	$31, $31, $0
	.livereg	0x807F0002,0x3FC00000
	ret	$31, ($26), 1
$61:
	ldiq	$19, 1
	.loc	2 496
 #  495	
 #  496	    while (nl > 0 && !((*nn++)--)) 
	cmpule	$19, $17, $0
	beq	$0, $63
	ldq	$18, 0($1)
	cmpeq	$18, 0, $16
	addq	$18, -1, $3
	stq	$3, 0($1)
	addq	$1, 8, $1
	beq	$16, $63
$62:
	.loc	2 497
 #  497	        nl--;
	addq	$17, -1, $17
	cmpule	$19, $17, $0
	beq	$0, $63
	ldq	$18, 0($1)
	cmpeq	$18, 0, $16
	addq	$18, -1, $4
	stq	$4, 0($1)
	addq	$1, 8, $1
	bne	$16, $62
$63:
	.loc	2 499
 #  498	
 #  499	    return (nl > 0 ? 1 : 0);
	ldil	$16, 1
	cmoveq	$0, 0, $16
	bis	$16, $16, $0
$64:
	.livereg	0xFC7F0002,0x3FC00000
	ret	$31, ($26), 1
	.end	BnnSubtractBorrow
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnSubtract
	.loc	2 514
 #  514	{
	.ent	BnnSubtract 2
BnnSubtract:
	.option	O2
	ldgp	$gp, 0($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)
	.mask	0x04000000, -16
	.frame	$sp, 16, $26, 0
	.prologue	1
	bis	$16, $16, $1
	bis	$17, $17, $2
	.loc	2 514

	.loc	2 515
 #  515	    register BigNumProduct 	c = carryin;
	bis	$20, $20, $21
	.loc	2 519
 #  516	    register BigNumDigit 	invn;
 #  517	
 #  518	
 #  519	    ml -= nl;
	subq	$2, $19, $2
	.loc	2 521
 #  520	    /* test computed at compile time */
 #  521	    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
	ldiq	$17, 1
	.loc	2 533
 #  533	    {
	.loc	2 536
 #  534		register BigNumProduct save;
 #  535	
 #  536		while (nl > 0) 
	cmpult	$19, $17, $3
	bne	$3, $68
$65:
	.loc	2 537
 #  537		{
	.loc	2 538
 #  538		    save = *mm;
	ldq	$0, 0($1)
	.loc	2 539
 #  539		    invn = *(nn++) ^ -1;
	ldq	$16, 0($18)
	xor	$16, -1, $16
	addq	$18, 8, $18
	.loc	2 540
 #  540		    c += save;
	addq	$21, $0, $21
	.loc	2 542
 #  541	
 #  542		    if (c < save)
	cmpult	$21, $0, $4
	beq	$4, $66
	.loc	2 543
 #  543		    {
	.loc	2 544
 #  544			*(mm++) = invn;
	stq	$16, 0($1)
	addq	$1, 8, $1
	.loc	2 545
 #  545			c = 1;
	bis	$17, $17, $21
	br	$31, $67
$66:
	.loc	2 548
 #  546		    }
 #  547		    else
 #  548		    {
	.loc	2 549
 #  549			c += invn;
	addq	$21, $16, $21
	.loc	2 550
 #  550	 		*(mm++) = c;
	stq	$21, 0($1)
	addq	$1, 8, $1
	.loc	2 551
 #  551	 		c = (c < invn) ? 1 : 0;
	cmpult	$21, $16, $0
	ldiq	$21, 1
	cmoveq	$0, 0, $21
$67:
	.loc	2 553
 #  552		    }
 #  553		    nl--;
	addq	$19, -1, $19
	cmpult	$19, $17, $5
	beq	$5, $65
$68:
	.loc	2 557
 #  554	 	}
 #  555	    }
 #  556	
 #  557	    return (BnnSubtractBorrow (mm, ml, (BigNumCarry) c)); }
	bis	$1, $1, $16
	bis	$2, $2, $17
	bis	$21, $21, $18
	.livereg	0x0001F002,0x00000000
	jsr	$26, BnnSubtractBorrow
	ldgp	$gp, 0($26)
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end	BnnSubtract
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnMultiplyDigit
	.loc	2 577
 #  577	{ 
	.ent	BnnMultiplyDigit 2
BnnMultiplyDigit:
	.option	O2
	ldgp	$gp, 0($27)
	lda	$sp, -16($sp)
	stq	$26, 0($sp)
	.mask	0x04000000, -16
	.frame	$sp, 16, $26, 0
	.prologue	1
	bis	$16, $16, $1
	bis	$17, $17, $3
	.loc	2 577

	.loc	2 578
 #  578	    register BigNumProduct c = 0;
	bis	$31, $31, $21
	.loc	2 581
 #  579	
 #  580	    
 #  581	    if (d == 0) 
	bne	$20, $69
	.loc	2 582
 #  582	        return (0);
	bis	$31, $31, $0
	br	$31, $77
$69:
	.loc	2 584
 #  583	
 #  584	    if (d == 1) 
	subq	$20, 1, $4
	bne	$4, $70
	.loc	2 585
 #  585	        return (BnnAdd (pp, pl, mm, ml, (BigNumCarry) 0));
	bis	$1, $1, $16
	bis	$3, $3, $17
	bis	$31, $31, $20
	.livereg	0x0001FC02,0x00000000
	jsr	$26, BnnAdd
	ldgp	$gp, 0($26)
	br	$31, $77
$70:
	.loc	2 587
 #  586	
 #  587	    pl -= ml;
	subq	$3, $19, $3
	.loc	2 589
 #  588	    /* test computed at compile time */
 #  589	    if (sizeof (BigNumProduct) > sizeof (BigNumDigit)) 
	.loc	2 610
 #  610	    {
	.loc	2 613
 #  611	#ifdef __alpha /* _hack_to_produce_east_to_modify_assembler */
 #  612		register BigNumDigit X0, m_digit,Lo,Hi;
 #  613		while (ml != 0) 
	beq	$19, $73
	and	$19, 3, $0
	negq	$0, $0
	bis	$0, $0, $2
	beq	$2, $72
	addq	$0, $19, $2
$71:
	addq	$19, -1, $19
	ldq	$0, 0($1)
	ldq	$16, 0($18)
	addq	$18, 8, $18
	addq	$0, $21, $0
	cmpult	$0, $21, $21
	mulq	$20, $16, $5
	addq	$5, $0, $17
	cmpult	$17, $0, $6
	addq	$21, $6, $21
	stq	$17, 0($1)
	addq	$1, 8, $1
	umulh	$20, $16, $7
	addq	$21, $7, $21
	subq	$2, $19, $8
	bne	$8, $71
	beq	$19, $73
$72:
	.loc	2 614
 #  614		{
	.loc	2 615
 #  615		    ml--;
	.loc	2 616
 #  616		    X0 = *pp;
	ldq	$0, 0($1)
	.loc	2 617
 #  617		    m_digit = *(mm++);
	ldq	$16, 0($18)
	addq	$18, 8, $18
	.loc	2 618
 #  618		    X0 += c;
	addq	$0, $21, $0
	.loc	2 619
 #  619		    c = X0 < c;
	cmpult	$0, $21, $21
	.loc	2 620
 #  620		    Lo = X0 + (d * m_digit);
	.loc	2 621
 #  621		    c += Lo < X0;
	mulq	$20, $16, $22
	addq	$22, $0, $17
	cmpult	$17, $0, $23
	addq	$21, $23, $21
	.loc	2 622
 #  622		    *(pp++) = Lo;
	stq	$17, 0($1)
	addq	$1, 8, $1
	.loc	2 623
 #  623		    c += asm("umulh %a0, %a1, %v0",d,m_digit);
	umulh	$20, $16, $24
	addq	$21, $24, $21
	ldq	$0, 0($1)
	ldq	$16, 0($18)
	addq	$18, 8, $18
	addq	$0, $21, $0
	cmpult	$0, $21, $21
	mulq	$20, $16, $25
	addq	$25, $0, $17
	cmpult	$17, $0, $27
	addq	$21, $27, $21
	stq	$17, 0($1)
	addq	$1, 8, $1
	umulh	$20, $16, $4
	addq	$21, $4, $21
	ldq	$0, 0($1)
	ldq	$16, 0($18)
	addq	$18, 8, $18
	addq	$0, $21, $0
	cmpult	$0, $21, $21
	mulq	$20, $16, $5
	addq	$5, $0, $17
	cmpult	$17, $0, $6
	addq	$21, $6, $21
	stq	$17, 0($1)
	addq	$1, 8, $1
	umulh	$20, $16, $7
	addq	$21, $7, $21
	addq	$19, -4, $19
	ldq	$0, 0($1)
	ldq	$16, 0($18)
	addq	$18, 8, $18
	addq	$0, $21, $0
	cmpult	$0, $21, $21
	mulq	$20, $16, $8
	addq	$8, $0, $17
	cmpult	$17, $0, $22
	addq	$21, $22, $21
	stq	$17, 0($1)
	addq	$1, 8, $1
	umulh	$20, $16, $23
	addq	$21, $23, $21
	bne	$19, $72
$73:
	.loc	2 661
 #  661		X0 = *pp;
	ldq	$0, 0($1)
	.loc	2 662
 #  662		c += X0;
	addq	$21, $0, $21
	.loc	2 663
 #  663		*(pp++) = c;
	stq	$21, 0($1)
	addq	$1, 8, $1
	.loc	2 665
 #  664	
 #  665		if (c >= X0)
	cmpult	$21, $0, $24
	bne	$24, $74
	.loc	2 666
 #  666		    return (0);
	bis	$31, $31, $0
	br	$31, $77
$74:
	.loc	2 668
 #  667	
 #  668		pl--;
	addq	$3, -1, $3
	.loc	2 669
 #  669		while (pl != 0 && !(++(*pp++))) 
	cmpeq	$3, 0, $0
	xor	$0, 1, $0
	beq	$0, $76
	ldq	$25, 0($1)
	addq	$25, 1, $27
	stq	$27, 0($1)
	ldq	$16, 0($1)
	cmpeq	$16, 0, $16
	addq	$1, 8, $1
	beq	$16, $76
$75:
	.loc	2 670
 #  670		    pl--;
	addq	$3, -1, $3
	cmpeq	$3, 0, $0
	xor	$0, 1, $0
	beq	$0, $76
	ldq	$4, 0($1)
	addq	$4, 1, $5
	stq	$5, 0($1)
	ldq	$16, 0($1)
	cmpeq	$16, 0, $16
	addq	$1, 8, $1
	bne	$16, $75
$76:
	.loc	2 672
 #  671	
 #  672		return (pl != 0 ? 0 : 1);
	bis	$31, $31, $16
	cmoveq	$0, 1, $16
	bis	$16, $16, $0
$77:
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	lda	$sp, 16($sp)
	ret	$31, ($26), 1
	.end	BnnMultiplyDigit
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnMultiply2Digit
	.loc	2 704
 #  704	{ 
	.ent	BnnMultiply2Digit 2
BnnMultiply2Digit:
	.option	O2
	ldgp	$gp, 0($27)
	lda	$sp, -416($sp)
	stq	$26, 0($sp)
	stq	$9, 8($sp)
	stq	$10, 16($sp)
	stq	$11, 24($sp)
	stq	$12, 32($sp)
	stq	$13, 40($sp)
	stq	$14, 48($sp)
	stq	$15, 56($sp)
	.mask	0x0400FE00, -416
	.frame	$sp, 416, $26, 48
	.prologue	1
	bis	$16, $16, $11
	stq	$17, 376($sp)
	bis	$18, $18, $9
	bis	$20, $20, $14
	bis	$21, $21, $15
	.loc	2 704

	.loc	2 706
 #  705	    BigNumDigit c0, c1, p0, p1;
 #  706	    if ((ml & 1))
	blbc	$19, $78
	.loc	2 707
 #  707	    {
	.loc	2 708
 #  708		return
	bis	$11, $11, $16
	ldq	$17, 376($sp)
	bis	$9, $9, $18
	bis	$14, $14, $20
	stq	$19, 392($sp)
	.livereg	0x0001FC02,0x00000000
	jsr	$26, BnnMultiplyDigit
	ldgp	$gp, 0($26)
	ldq	$19, 392($sp)
	bis	$0, $0, $10
	addq	$11, 8, $16
	ldq	$17, 376($sp)
	addq	$17, -1, $17
	bis	$9, $9, $18
	bis	$15, $15, $20
	.livereg	0x0001FC02,0x00000000
	jsr	$26, BnnMultiplyDigit
	ldgp	$gp, 0($26)
	addq	$0, $10, $0
	br	$31, $90
$78:
	.loc	2 712
 #  709		    BnnMultiplyDigit (pp, pl, mm, ml, d0)
 #  710		    + BnnMultiplyDigit (pp+1, pl-1, mm, ml, d1);
 #  711	    }
 #  712	    c0 = c1 = 0;
	bis	$31, $31, $12
	bis	$31, $31, $0
	.loc	2 725
 #  725	    if (d0 >= d1)
	cmpult	$14, $15, $22
	bne	$22, $82
	.loc	2 726
 #  726	    {
	.loc	2 728
 #  727		BigNumDigit d0_1, c2, c3, ctmp1;
 #  728		d0_1 = d0-d1;
	subq	$14, $15, $13
	stq	$13, 328($sp)
	.loc	2 730
 #  729	
 #  730		while (ml != 0)
	beq	$19, $86
	stq	$19, 392($sp)
	stq	$9, 384($sp)
$79:
	ldq	$19, 392($sp)
	ldq	$9, 384($sp)
	.loc	2 731
 #  731		{
	.loc	2 733
 #  732		    BigNumDigit m0,m1;
 #  733		    m0 = mm[0];
	ldq	$7, 0($9)
	.loc	2 734
 #  734		    m1 = mm[1];
	ldq	$8, 8($9)
	.loc	2 735
 #  735		    if (m0 >= m1)
	cmpult	$7, $8, $23
	bne	$23, $80
	.loc	2 736
 #  736		    {
	.loc	2 740
 #  737			BigNumDigit m0_1;
 #  738			BigNumDigit d0m0l, d0m0h, d1m1l, d1m1h, dfl, dfh;
 #  739			BigNumDigit t0, t1, t2;
 #  740			d0m0l = d0*m0;
	.loc	2 741
 #  741			d0m0h = asm("umulh %a0, %a1, %v0", d0,m0);
	umulh	$14, $7, $5
	bis	$5, $5, $20
	.loc	2 742
 #  742			m0_1 = m0-m1;
	.loc	2 743
 #  743			d1m1l = d1*m1;
	.loc	2 744
 #  744			d1m1h = asm("umulh %a0, %a1, %v0", d1,m1);
	.loc	2 745
 #  745			dfl = d0_1*m0_1;
	.loc	2 746
 #  746			dfh = asm("umulh %a0, %a1, %v0", d0_1,m0_1);
	.loc	2 747
 #  747			p0 = pp[0];
	ldq	$6, 0($11)
	.loc	2 748
 #  748			p1 = pp[1];
	ldq	$21, 8($11)
	.loc	2 749
 #  749			p0 += c0;
	addq	$6, $0, $6
	.loc	2 750
 #  750			ctmp1 = p0 < c0;
	cmpult	$6, $0, $26
	.loc	2 751
 #  751			p1 += c1;
	addq	$21, $12, $21
	.loc	2 752
 #  752			c2 = p1 < c1;
	cmpult	$21, $12, $18
	.loc	2 753
 #  753			p1 += ctmp1;
	addq	$21, $26, $21
	.loc	2 754
 #  754			c2 += p1 < ctmp1;
	cmpult	$21, $26, $24
	addq	$18, $24, $18
	.loc	2 755
 #  755			p0 += d0m0l;
	mulq	$14, $7, $2
	addq	$6, $2, $6
	.loc	2 756
 #  756			c1 = p0 < d0m0l;
	.loc	2 758
 #  757			/* compute: t2:t1:t0 = d0*m0 + d1*m1 */
 #  758			t0 = d0m0l+d1m1l;
	.loc	2 759
 #  759			ctmp1 = t0 < d0m0l;
	.loc	2 760
 #  760			t1 = d0m0h+d1m1h;
	.loc	2 761
 #  761			t2 = t1 < d0m0h;
	.loc	2 762
 #  762			t1 += ctmp1;
	umulh	$15, $8, $10
	addq	$5, $10, $19
	mulq	$15, $8, $9
	addq	$2, $9, $4
	cmpult	$4, $2, $20
	addq	$19, $20, $16
	bis	$16, $16, $0
	.loc	2 763
 #  763			t2 += t1 < ctmp1;
	.loc	2 767
 #  764			/* t2:t1:t0 = d0*m0 + d1*m1 */
 #  765			/* dfh:dfl = d0*m0 + d1*m1 - d0*m1 - d1*m0 */
 #  766			/* compute: t2:t1:t0 = t2:t1:t0 - dfh:dfl */
 #  767			ctmp1 = t0 < dfl;
	.loc	2 768
 #  768			t0 -= dfl;
	.loc	2 769
 #  769			t2 -= t1 < dfh;
	subq	$7, $8, $1
	cmpult	$19, $5, $25
	cmpult	$16, $20, $27
	addq	$25, $27, $22
	ldq	$23, 328($sp)
	xor	$23, $1, $24
	cmpult	$16, $24, $25
	subq	$22, $25, $3
	.loc	2 770
 #  770			t1 -= dfh;
	umulh	$13, $1, $27
	subq	$0, $27, $0
	.loc	2 771
 #  771			t2 -= t1 < ctmp1;
	mulq	$13, $1, $16
	cmpult	$4, $16, $17
	cmpult	$0, $17, $23
	subq	$3, $23, $3
	.loc	2 772
 #  772			t1 -= ctmp1;
	subq	$0, $17, $0
	.loc	2 774
 #  773			/* t2:t1:t0 = d0*m1 + d1*m0 */
 #  774			ultra_parnoid(t0, t1, t2, d0, d1, m0, m1);
	.loc	2 775
 #  775			d0m0h += c1;
	cmpult	$6, $2, $24
	addq	$5, $24, $20
	.loc	2 776
 #  776			p1 += d0m0h;
	addq	$21, $20, $21
	.loc	2 777
 #  777			c2 += p1 < d0m0h;
	cmpult	$21, $20, $22
	addq	$18, $22, $18
	.loc	2 778
 #  778			p1 += t0;
	subq	$4, $16, $19
	addq	$21, $19, $21
	.loc	2 779
 #  779			c2 += p1 < t0;
	cmpult	$21, $19, $25
	addq	$18, $25, $18
	.loc	2 780
 #  780			t1 += c2;
	addq	$0, $18, $0
	.loc	2 781
 #  781			t2 += t1 < c2;
	cmpult	$0, $18, $27
	addq	$3, $27, $3
	.loc	2 782
 #  782			c2 = t1 + d1m1l;
	addq	$0, $9, $16
	bis	$16, $16, $18
	.loc	2 783
 #  783			c3 = t2 + d1m1h + (c2 < t1);
	addq	$3, $10, $23
	cmpult	$16, $0, $24
	addq	$23, $24, $19
	br	$31, $81
$80:
	.loc	2 786
 #  784		    }
 #  785		    else
 #  786		    {
	.loc	2 790
 #  787			BigNumDigit m0_1;
 #  788			BigNumDigit d0m0l, d0m0h, d1m1l, d1m1h, dfl, dfh;
 #  789			BigNumDigit t0, t1, t2;
 #  790			d0m0l = d0*m0;
	.loc	2 791
 #  791			d0m0h = asm("umulh %a0, %a1, %v0", d0,m0);
	umulh	$14, $7, $5
	bis	$5, $5, $20
	.loc	2 792
 #  792			m0_1 = -m0+m1;
	.loc	2 793
 #  793			d1m1l = d1*m1;
	.loc	2 794
 #  794			d1m1h = asm("umulh %a0, %a1, %v0", d1,m1);
	.loc	2 795
 #  795			dfl = d0_1*m0_1;
	.loc	2 796
 #  796			dfh = asm("umulh %a0, %a1, %v0", d0_1,m0_1);
	.loc	2 797
 #  797			p0 = pp[0];
	ldq	$6, 0($11)
	.loc	2 798
 #  798			p1 = pp[1];
	ldq	$21, 8($11)
	.loc	2 799
 #  799			p0 += c0;
	addq	$6, $0, $6
	.loc	2 800
 #  800			ctmp1 = p0 < c0;
	cmpult	$6, $0, $26
	.loc	2 801
 #  801			p1 += c1;
	addq	$21, $12, $21
	.loc	2 802
 #  802			c2 = p1 < c1;
	cmpult	$21, $12, $18
	.loc	2 803
 #  803			p1 += ctmp1;
	addq	$21, $26, $21
	.loc	2 804
 #  804			c2 += p1 < ctmp1;
	cmpult	$21, $26, $22
	addq	$18, $22, $18
	.loc	2 805
 #  805			p0 += d0m0l;
	mulq	$14, $7, $2
	addq	$6, $2, $6
	.loc	2 806
 #  806			c1 = p0 < d0m0l;
	.loc	2 807
 #  807			t0 = d0m0l+d1m1l;
	.loc	2 808
 #  808			ctmp1 = t0 < d0m0l;
	.loc	2 809
 #  809			t1 = d0m0h+d1m1h;
	.loc	2 810
 #  810			t2 = t1 < d0m0h;
	.loc	2 811
 #  811			t1 += ctmp1;
	.loc	2 812
 #  812			t2 += t1 < ctmp1;
	umulh	$15, $8, $10
	addq	$5, $10, $19
	mulq	$15, $8, $9
	addq	$2, $9, $4
	cmpult	$4, $2, $20
	addq	$19, $20, $16
	cmpult	$19, $5, $25
	cmpult	$16, $20, $27
	addq	$25, $27, $1
	.loc	2 816
 #  813			/* t2:t1:t0 = d0*m0 + d1*m1 */
 #  814			/* dfh:dfl = - d0*m0 - d1*m1 + d0*m1 + d1*m0 */
 #  815			/* compute: t2:t1:t0 = t2:t1:t0 + dfh:dfl */
 #  816			t0 += dfl;
	.loc	2 817
 #  817			ctmp1 = t0 < dfl;
	.loc	2 818
 #  818			t1 += dfh;
	subq	$8, $7, $3
	ldq	$23, 328($sp)
	xor	$23, $3, $24
	addq	$16, $24, $17
	.loc	2 819
 #  819			t2 += t1 < dfh;
	umulh	$13, $3, $22
	cmpult	$17, $22, $25
	addq	$1, $25, $1
	.loc	2 820
 #  820			t1 += ctmp1;
	mulq	$13, $3, $16
	addq	$4, $16, $0
	cmpult	$0, $16, $19
	addq	$17, $19, $17
	.loc	2 821
 #  821			t2 += t1 < ctmp1;
	cmpult	$17, $19, $27
	addq	$1, $27, $1
	.loc	2 823
 #  822			/* t2:t1:t0 = d0*m1 + d1*m0 */
 #  823			ultra_parnoid(t0, t1, t2, d0, d1, m0, m1);
	.loc	2 824
 #  824			d0m0h += c1;
	cmpult	$6, $2, $23
	addq	$5, $23, $20
	.loc	2 825
 #  825			p1 += d0m0h;
	addq	$21, $20, $21
	.loc	2 826
 #  826			c2 += p1 < d0m0h;
	cmpult	$21, $20, $24
	addq	$18, $24, $18
	.loc	2 827
 #  827			p1 += t0;
	addq	$21, $0, $21
	.loc	2 828
 #  828			c2 += p1 < t0;
	cmpult	$21, $0, $22
	addq	$18, $22, $18
	.loc	2 829
 #  829			t1 += c2;
	addq	$17, $18, $17
	.loc	2 830
 #  830			t2 += t1 < c2;
	cmpult	$17, $18, $25
	addq	$1, $25, $1
	.loc	2 831
 #  831			c2 = t1 + d1m1l;
	addq	$17, $9, $0
	bis	$0, $0, $18
	.loc	2 832
 #  832			c3 = t2 + d1m1h + (c2 < t1);
	addq	$1, $10, $27
	cmpult	$0, $17, $23
	addq	$27, $23, $19
$81:
	ldq	$16, 392($sp)
	.loc	2 835
 #  833		    }
 #  834	
 #  835		    pp[0] = p0;
	stq	$6, 0($11)
	.loc	2 836
 #  836		    pp[1] = p1;
	stq	$21, 8($11)
	.loc	2 837
 #  837		    pp += 2;
	addq	$11, 16, $11
	.loc	2 838
 #  838		    pl -= 2;
	ldq	$24, 376($sp)
	addq	$24, -2, $22
	stq	$22, 376($sp)
	.loc	2 839
 #  839		    c0 = c2;
	bis	$18, $18, $0
	.loc	2 840
 #  840		    c1 = c3;
	bis	$19, $19, $12
	.loc	2 841
 #  841		    ml -= 2;
	addq	$16, -2, $16
	.loc	2 842
 #  842		    mm += 2;
	ldq	$25, 384($sp)
	addq	$25, 16, $27
	stq	$27, 384($sp)
	stq	$16, 392($sp)
	bne	$16, $79
	br	$31, $86
$82:
	.loc	2 846
 #  843		}
 #  844	    }
 #  845	    else
 #  846	    {
	.loc	2 848
 #  847		BigNumDigit d0_1, c2, c3, ctmp1;
 #  848		d0_1 = d1-d0;
	subq	$15, $14, $13
	stq	$13, 120($sp)
	.loc	2 850
 #  849	
 #  850		while (ml != 0)
	beq	$19, $86
	stq	$19, 392($sp)
	stq	$9, 384($sp)
$83:
	ldq	$19, 392($sp)
	ldq	$9, 384($sp)
	.loc	2 851
 #  851		{
	.loc	2 853
 #  852		    BigNumDigit m0,m1;
 #  853		    m0 = mm[0];
	ldq	$7, 0($9)
	.loc	2 854
 #  854		    m1 = mm[1];
	ldq	$8, 8($9)
	.loc	2 855
 #  855		    if (m0 >= m1)
	cmpult	$7, $8, $23
	bne	$23, $84
	.loc	2 856
 #  856		    {
	.loc	2 860
 #  857			BigNumDigit m0_1;
 #  858			BigNumDigit d0m0l, d0m0h, d1m1l, d1m1h, dfl, dfh;
 #  859			BigNumDigit t0, t1, t2;
 #  860			d0m0l = d0*m0;
	.loc	2 861
 #  861			d0m0h = asm("umulh %a0, %a1, %v0", d0,m0);
	umulh	$14, $7, $5
	bis	$5, $5, $20
	.loc	2 862
 #  862			m0_1 = m0-m1;
	.loc	2 863
 #  863			d1m1l = d1*m1;
	.loc	2 864
 #  864			d1m1h = asm("umulh %a0, %a1, %v0", d1,m1);
	.loc	2 865
 #  865			dfl = d0_1*m0_1;
	.loc	2 866
 #  866			dfh = asm("umulh %a0, %a1, %v0", d0_1,m0_1);
	.loc	2 867
 #  867			p0 = pp[0];
	ldq	$6, 0($11)
	.loc	2 868
 #  868			p1 = pp[1];
	ldq	$21, 8($11)
	.loc	2 869
 #  869			p0 += c0;
	addq	$6, $0, $6
	.loc	2 870
 #  870			ctmp1 = p0 < c0;
	cmpult	$6, $0, $26
	.loc	2 871
 #  871			p1 += c1;
	addq	$21, $12, $21
	.loc	2 872
 #  872			c2 = p1 < c1;
	cmpult	$21, $12, $18
	.loc	2 873
 #  873			p1 += ctmp1;
	addq	$21, $26, $21
	.loc	2 874
 #  874			c2 += p1 < ctmp1;
	cmpult	$21, $26, $24
	addq	$18, $24, $18
	.loc	2 875
 #  875			p0 += d0m0l;
	mulq	$14, $7, $2
	addq	$6, $2, $6
	.loc	2 876
 #  876			c1 = p0 < d0m0l;
	.loc	2 878
 #  877			/* compute: t2:t1:t0 = d0*m0 + d1*m1 */
 #  878			t0 = d0m0l+d1m1l;
	.loc	2 879
 #  879			ctmp1 = t0 < d0m0l;
	.loc	2 880
 #  880			t1 = d0m0h+d1m1h;
	.loc	2 881
 #  881			t2 = t1 < d0m0h;
	.loc	2 882
 #  882			t1 += ctmp1;
	.loc	2 883
 #  883			t2 += t1 < ctmp1;
	umulh	$15, $8, $10
	addq	$5, $10, $19
	mulq	$15, $8, $9
	addq	$2, $9, $4
	cmpult	$4, $2, $20
	addq	$19, $20, $16
	cmpult	$19, $5, $22
	cmpult	$16, $20, $25
	addq	$22, $25, $1
	.loc	2 887
 #  884			/* t2:t1:t0 = d0*m0 + d1*m1 */
 #  885			/* dfh:dfl = - d0*m0 - d1*m1 + d0*m1 + d1*m0 */
 #  886			/* compute: t2:t1:t0 = t2:t1:t0 + dfh:dfl */
 #  887			t0 += dfl;
	.loc	2 888
 #  888			ctmp1 = t0 < dfl;
	.loc	2 889
 #  889			t1 += dfh;
	subq	$7, $8, $3
	ldq	$27, 120($sp)
	xor	$27, $3, $23
	addq	$16, $23, $17
	.loc	2 890
 #  890			t2 += t1 < dfh;
	umulh	$13, $3, $24
	cmpult	$17, $24, $22
	addq	$1, $22, $1
	.loc	2 891
 #  891			t1 += ctmp1;
	mulq	$13, $3, $16
	addq	$4, $16, $0
	cmpult	$0, $16, $19
	addq	$17, $19, $17
	.loc	2 892
 #  892			t2 += t1 < ctmp1;
	cmpult	$17, $19, $25
	addq	$1, $25, $1
	.loc	2 894
 #  893			/* t2:t1:t0 = d0*m1 + d1*m0 */
 #  894			ultra_parnoid(t0, t1, t2, d0, d1, m0, m1);
	.loc	2 895
 #  895			d0m0h += c1;
	cmpult	$6, $2, $27
	addq	$5, $27, $20
	.loc	2 896
 #  896			p1 += d0m0h;
	addq	$21, $20, $21
	.loc	2 897
 #  897			c2 += p1 < d0m0h;
	cmpult	$21, $20, $23
	addq	$18, $23, $18
	.loc	2 898
 #  898			p1 += t0;
	addq	$21, $0, $21
	.loc	2 899
 #  899			c2 += p1 < t0;
	cmpult	$21, $0, $24
	addq	$18, $24, $18
	.loc	2 900
 #  900			t1 += c2;
	addq	$17, $18, $17
	.loc	2 901
 #  901			t2 += t1 < c2;
	cmpult	$17, $18, $22
	addq	$1, $22, $1
	.loc	2 902
 #  902			c2 = t1 + d1m1l;
	addq	$17, $9, $0
	bis	$0, $0, $18
	.loc	2 903
 #  903			c3 = t2 + d1m1h + (c2 < t1);
	addq	$1, $10, $25
	cmpult	$0, $17, $27
	addq	$25, $27, $19
	br	$31, $85
$84:
	.loc	2 906
 #  904		    }
 #  905		    else
 #  906		    {
	.loc	2 910
 #  907			BigNumDigit m0_1;
 #  908			BigNumDigit d0m0l, d0m0h, d1m1l, d1m1h, dfl, dfh;
 #  909			BigNumDigit t0, t1, t2;
 #  910			d0m0l = d0*m0;
	.loc	2 911
 #  911			d0m0h = asm("umulh %a0, %a1, %v0", d0,m0);
	umulh	$14, $7, $5
	bis	$5, $5, $20
	.loc	2 912
 #  912			m0_1 = -m0+m1;
	.loc	2 913
 #  913			d1m1l = d1*m1;
	.loc	2 914
 #  914			d1m1h = asm("umulh %a0, %a1, %v0", d1,m1);
	.loc	2 915
 #  915			dfl = d0_1*m0_1;
	.loc	2 916
 #  916			dfh = asm("umulh %a0, %a1, %v0", d0_1,m0_1);
	.loc	2 917
 #  917			p0 = pp[0];
	ldq	$6, 0($11)
	.loc	2 918
 #  918			p1 = pp[1];
	ldq	$21, 8($11)
	.loc	2 919
 #  919			p0 += c0;
	addq	$6, $0, $6
	.loc	2 920
 #  920			ctmp1 = p0 < c0;
	cmpult	$6, $0, $26
	.loc	2 921
 #  921			p1 += c1;
	addq	$21, $12, $21
	.loc	2 922
 #  922			c2 = p1 < c1;
	cmpult	$21, $12, $18
	.loc	2 923
 #  923			p1 += ctmp1;
	addq	$21, $26, $21
	.loc	2 924
 #  924			c2 += p1 < ctmp1;
	cmpult	$21, $26, $23
	addq	$18, $23, $18
	.loc	2 925
 #  925			p0 += d0m0l;
	mulq	$14, $7, $2
	addq	$6, $2, $6
	.loc	2 926
 #  926			c1 = p0 < d0m0l;
	.loc	2 927
 #  927			t0 = d0m0l+d1m1l;
	.loc	2 928
 #  928			ctmp1 = t0 < d0m0l;
	.loc	2 929
 #  929			t1 = d0m0h+d1m1h;
	.loc	2 930
 #  930			t2 = t1 < d0m0h;
	.loc	2 931
 #  931			t1 += ctmp1;
	umulh	$15, $8, $10
	addq	$5, $10, $19
	mulq	$15, $8, $9
	addq	$2, $9, $4
	cmpult	$4, $2, $20
	addq	$19, $20, $16
	bis	$16, $16, $0
	.loc	2 932
 #  932			t2 += t1 < ctmp1;
	.loc	2 936
 #  933			/* t2:t1:t0 = d0*m0 + d1*m1 */
 #  934			/* dfh:dfl = d0*m0 + d1*m1 - d0*m1 - d1*m0 */
 #  935			/* compute: t2:t1:t0 = t2:t1:t0 - dfh:dfl */
 #  936			ctmp1 = t0 < dfl;
	.loc	2 937
 #  937			t0 -= dfl;
	.loc	2 938
 #  938			t2 -= t1 < dfh;
	subq	$8, $7, $1
	cmpult	$19, $5, $24
	cmpult	$16, $20, $22
	addq	$24, $22, $25
	ldq	$27, 120($sp)
	xor	$27, $1, $23
	cmpult	$16, $23, $24
	subq	$25, $24, $3
	.loc	2 939
 #  939			t1 -= dfh;
	umulh	$13, $1, $22
	subq	$0, $22, $0
	.loc	2 940
 #  940			t2 -= t1 < ctmp1;
	mulq	$13, $1, $16
	cmpult	$4, $16, $17
	cmpult	$0, $17, $27
	subq	$3, $27, $3
	.loc	2 941
 #  941			t1 -= ctmp1;
	subq	$0, $17, $0
	.loc	2 943
 #  942			/* t2:t1:t0 = d0*m1 + d1*m0 */
 #  943			ultra_parnoid(t0, t1, t2, d0, d1, m0, m1);
	.loc	2 944
 #  944			d0m0h += c1;
	cmpult	$6, $2, $23
	addq	$5, $23, $20
	.loc	2 945
 #  945			p1 += d0m0h;
	addq	$21, $20, $21
	.loc	2 946
 #  946			c2 += p1 < d0m0h;
	cmpult	$21, $20, $25
	addq	$18, $25, $18
	.loc	2 947
 #  947			p1 += t0;
	subq	$4, $16, $19
	addq	$21, $19, $21
	.loc	2 948
 #  948			c2 += p1 < t0;
	cmpult	$21, $19, $24
	addq	$18, $24, $18
	.loc	2 949
 #  949			t1 += c2;
	addq	$0, $18, $0
	.loc	2 950
 #  950			t2 += t1 < c2;
	cmpult	$0, $18, $22
	addq	$3, $22, $3
	.loc	2 951
 #  951			c2 = t1 + d1m1l;
	addq	$0, $9, $16
	bis	$16, $16, $18
	.loc	2 952
 #  952			c3 = t2 + d1m1h + (c2 < t1);
	addq	$3, $10, $27
	cmpult	$16, $0, $23
	addq	$27, $23, $19
$85:
	ldq	$16, 392($sp)
	.loc	2 955
 #  953		    }
 #  954	
 #  955		    pp[0] = p0;
	stq	$6, 0($11)
	.loc	2 956
 #  956		    pp[1] = p1;
	stq	$21, 8($11)
	.loc	2 957
 #  957		    pp += 2;
	addq	$11, 16, $11
	.loc	2 958
 #  958		    pl -= 2;
	ldq	$25, 376($sp)
	addq	$25, -2, $24
	stq	$24, 376($sp)
	.loc	2 959
 #  959		    c0 = c2;
	bis	$18, $18, $0
	.loc	2 960
 #  960		    c1 = c3;
	bis	$19, $19, $12
	.loc	2 961
 #  961		    ml -= 2;
	addq	$16, -2, $16
	.loc	2 962
 #  962		    mm += 2;
	ldq	$22, 384($sp)
	addq	$22, 16, $27
	stq	$27, 384($sp)
	stq	$16, 392($sp)
	bne	$16, $83
$86:
	.loc	2 965
 #  963		}
 #  964	    }
 #  965	    p0 = pp[0];
	ldq	$6, 0($11)
	.loc	2 966
 #  966	    p1 = pp[1];
	ldq	$21, 8($11)
	.loc	2 967
 #  967	    p0 += c0;
	addq	$6, $0, $6
	.loc	2 968
 #  968	    pp[0] = p0;
	stq	$6, 0($11)
	.loc	2 969
 #  969	    c1 += p0 < c0;
	cmpult	$6, $0, $23
	addq	$12, $23, $12
	.loc	2 970
 #  970	    p1 += c1;
	addq	$21, $12, $21
	.loc	2 971
 #  971	    pp[1] = p1;
	stq	$21, 8($11)
	.loc	2 973
 #  972	
 #  973	    if (c1 <= p1)
	cmpult	$21, $12, $25
	bne	$25, $87
	.loc	2 974
 #  974	    {
	.loc	2 978
 #  975	#ifdef PARANOID
 #  976		assert(sc == 0 && BnnCompare(sp, sl, rp, sl) == BN_EQ);
 #  977	#endif
 #  978		return (0);
	bis	$31, $31, $0
	br	$31, $90
$87:
	ldq	$17, 376($sp)
	.loc	2 981
 #  979	    }
 #  980	
 #  981	    pl -= 2;
	addq	$17, -2, $17
	.loc	2 982
 #  982	    pp+=2;
	addq	$11, 16, $11
	.loc	2 983
 #  983	    while (pl != 0 && !(++(*pp++))) 
	cmpeq	$17, 0, $0
	xor	$0, 1, $0
	beq	$0, $89
	ldq	$24, 0($11)
	addq	$24, 1, $22
	stq	$22, 0($11)
	ldq	$16, 0($11)
	cmpeq	$16, 0, $16
	addq	$11, 8, $11
	beq	$16, $89
$88:
	.loc	2 984
 #  984		pl--;
	addq	$17, -1, $17
	cmpeq	$17, 0, $0
	xor	$0, 1, $0
	beq	$0, $89
	ldq	$27, 0($11)
	addq	$27, 1, $23
	stq	$23, 0($11)
	ldq	$16, 0($11)
	cmpeq	$16, 0, $16
	addq	$11, 8, $11
	bne	$16, $88
$89:
	.loc	2 990
 #  985	
 #  986	#ifdef PARANOID
 #  987	    assert(sc == (pl != 0 ? 0 : 1));
 #  988	    assert(BnnCompare(sp, sl, rp, sl) == BN_EQ);
 #  989	#endif
 #  990	    return (pl != 0 ? 0 : 1);
	bis	$31, $31, $16
	cmoveq	$0, 1, $16
	bis	$16, $16, $0
$90:
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	ldq	$9, 8($sp)
	ldq	$10, 16($sp)
	ldq	$11, 24($sp)
	ldq	$12, 32($sp)
	ldq	$13, 40($sp)
	ldq	$14, 48($sp)
	ldq	$15, 56($sp)
	lda	$sp, 416($sp)
	ret	$31, ($26), 1
	.end	BnnMultiply2Digit
	.text	
	.align	4
	.file	2 "c/KerN.c"
	.globl	BnnDivideDigit
	.loc	2 1019
 # 1019	{
	.ent	BnnDivideDigit 2
BnnDivideDigit:
	.option	O2
	ldgp	$gp, 0($27)
	lda	$sp, -240($sp)
	stq	$26, 0($sp)
	stq	$9, 8($sp)
	stq	$10, 16($sp)
	stq	$11, 24($sp)
	stq	$12, 32($sp)
	.mask	0x04001E00, -240
	.frame	$sp, 240, $26, 48
	.prologue	1
	bis	$16, $16, $10
	bis	$17, $17, $12
	bis	$18, $18, $11
	bis	$19, $19, $5
	.loc	2 1019

	.loc	2 1021
 # 1020	    /* test computed at compile time */
 # 1021	    if (sizeof (BigNumProduct) > sizeof (BigNumDigit))
	.loc	2 1042
 # 1042	    {
	.loc	2 1053
 # 1053		k = BnnNumLeadingZeroBitsInDigit (d);
	bis	$5, $5, $16
	stq	$5, 216($sp)
	.livereg	0x0001C002,0x00000000
	jsr	$26, BnnNumLeadingZeroBitsInDigit
	ldgp	$gp, 0($26)
	ldq	$5, 216($sp)
	addl	$0, 0, $16
	stl	$16, 176($sp)
	.loc	2 1054
 # 1054		if (k != 0) 
	beq	$16, $91
	.loc	2 1055
 # 1055		{
	.loc	2 1056
 # 1056		    prev_qq = qq[-1];
	ldq	$22, -8($10)
	stq	$22, 104($sp)
	.loc	2 1057
 # 1057		    orig_nl = nl;
	stq	$11, 168($sp)
	.loc	2 1058
 # 1058		    d <<= k;
	ldl	$23, 176($sp)
	sll	$5, $23, $5
	.loc	2 1059
 # 1059		    BnnShiftLeft (nn, nl, k);    
	bis	$12, $12, $16
	bis	$11, $11, $17
	bis	$23, $23, $18
	stq	$5, 216($sp)
	.livereg	0x0001E002,0x00000000
	jsr	$26, BnnShiftLeft
	ldgp	$gp, 0($26)
	ldq	$5, 216($sp)
$91:
	.loc	2 1062
 # 1060		}
 # 1061	
 # 1062		nn += nl;
	s8addq	$11, $12, $12
	.loc	2 1063
 # 1063		nl--;
	addq	$11, -1, $11
	.loc	2 1064
 # 1064		qq += nl;
	s8addq	$11, $10, $10
	.loc	2 1066
 # 1065	
 # 1066		ch = HIGH (d);
	srl	$5, 32, $6
	bis	$6, $6, $26
	.loc	2 1067
 # 1067		cl = LOW (d);
	and	$5, 4294967295, $8
	bis	$8, $8, $9
	.loc	2 1069
 # 1068	
 # 1069		rl = *(--nn);
	addq	$12, -8, $12
	ldq	$7, 0($12)
	.loc	2 1071
 # 1070	
 # 1071		while (nl != 0)
	beq	$11, $103
$92:
	.loc	2 1072
 # 1072		{
	.loc	2 1073
 # 1073		    nl--;
	addq	$11, -1, $11
	.loc	2 1074
 # 1074		    rh = rl; 
	bis	$7, $7, $1
	.loc	2 1075
 # 1075		    rl = *(--nn);
	addq	$12, -8, $12
	ldq	$7, 0($12)
	.loc	2 1076
 # 1076		    qa = rh / ch; 	/* appr. quotient */
	divqu	$1, $6, $0
	bis	$0, $0, $3
	.loc	2 1079
 # 1077	
 # 1078		    /* Compute ph, pl */
 # 1079		    pl = cl * qa;
	.loc	2 1080
 # 1080		    ph = ch * qa;
	.loc	2 1081
 # 1081		    ph += HIGH (pl);
	mulq	$9, $0, $18
	mulq	$6, $0, $24
	srl	$18, 32, $25
	addq	$24, $25, $19
	bis	$19, $19, $17
	.loc	2 1082
 # 1082		    pl = L2H (pl);
	sll	$18, 32, $20
	bis	$20, $20, $16
	.loc	2 1085
 # 1083	
 # 1084		    /* While ph:pl > rh:rl, decrement qa, adjust qh:ql */
 # 1085		    while (ph > rh || ph == rh && pl > rl) 
	cmpult	$1, $19, $27
	bne	$27, $93
	divqu	$1, $26, $0
	mulq	$26, $0, $22
	mulq	$9, $0, $23
	srl	$23, 32, $24
	addq	$22, $24, $25
	subq	$25, $1, $27
	bne	$27, $96
	cmpult	$7, $20, $23
	beq	$23, $96
$93:
	.loc	2 1086
 # 1086		    {
	.loc	2 1087
 # 1087			qa--;
	addq	$3, -1, $3
	.loc	2 1088
 # 1088			SUB (ph, pl, ch, L2H (cl));
	sll	$8, 32, $0
	cmpult	$16, $0, $22
	beq	$22, $94
	.loc	2 1088

	.loc	2 1088

	subq	$16, $0, $16
	.loc	2 1088

	subq	$17, $6, $17
	addq	$17, -1, $17
	br	$31, $95
$94:
	.loc	2 1088

	.loc	2 1088

	subq	$16, $0, $16
	.loc	2 1088

	subq	$17, $6, $17
$95:
	.loc	2 1088

	cmpult	$1, $17, $24
	bne	$24, $93
	subq	$17, $1, $25
	bne	$25, $96
	cmpult	$7, $16, $27
	bne	$27, $93
$96:
	.loc	2 1091
 # 1089		    }
 # 1090	
 # 1091		    SUB (rh, rl, ph, pl);
	cmpult	$7, $16, $23
	beq	$23, $97
	.loc	2 1091

	.loc	2 1091

	subq	$7, $16, $7
	.loc	2 1091

	subq	$1, $17, $1
	addq	$1, -1, $1
	br	$31, $98
$97:
	.loc	2 1091

	.loc	2 1091

	subq	$7, $16, $7
	.loc	2 1091

	subq	$1, $17, $1
$98:
	.loc	2 1091

	.loc	2 1094
 # 1092	
 # 1093		    /* Top half of quotient is correct; save it */
 # 1094		    *(--qq) = L2H (qa);
	addq	$10, -8, $18
	bis	$18, $18, $2
	bis	$18, $18, $10
	sll	$3, 32, $22
	stq	$22, 0($2)
	.loc	2 1095
 # 1095		    qa = (L2H (rh) | HIGH (rl)) / ch;
	sll	$1, 32, $24
	srl	$7, 32, $25
	or	$24, $25, $4
	divqu	$4, $6, $0
	bis	$0, $0, $3
	.loc	2 1099
 # 1096	
 # 1097		    /* Approx low half of q */
 # 1098		    /* Compute ph, pl, again */
 # 1099		    pl = cl * qa;
	.loc	2 1100
 # 1100		    ph = ch * qa;
	.loc	2 1101
 # 1101		    ph += HIGH (pl);
	.loc	2 1102
 # 1102		    pl = LOW (pl) | L2H (LOW (ph));
	mulq	$9, $0, $19
	mulq	$6, $0, $27
	srl	$19, 32, $23
	addq	$27, $23, $20
	and	$19, 4294967295, $22
	and	$20, 4294967295, $24
	sll	$24, 32, $25
	or	$22, $25, $16
	.loc	2 1103
 # 1103		    ph = HIGH (ph);
	srl	$20, 32, $21
	bis	$21, $21, $17
	.loc	2 1106
 # 1104	
 # 1105		    /* While ph:pl > rh:rl, decrement qa, adjust qh:ql */
 # 1106		    while (ph > rh || ph == rh && pl > rl)
	cmpult	$1, $21, $27
	bne	$27, $99
	divqu	$4, $26, $0
	mulq	$26, $0, $23
	mulq	$9, $0, $24
	srl	$24, 32, $22
	addq	$23, $22, $25
	srl	$25, 32, $27
	subq	$27, $1, $24
	bne	$24, $102
	cmpult	$7, $16, $23
	beq	$23, $102
$99:
	.loc	2 1107
 # 1107		    {
	.loc	2 1108
 # 1108			qa--;
	addq	$3, -1, $3
	.loc	2 1109
 # 1109			SUB (ph, pl, 0, d);
	cmpult	$16, $5, $22
	beq	$22, $100
	.loc	2 1109

	.loc	2 1109

	subq	$16, $5, $16
	.loc	2 1109

	addq	$17, -1, $17
	br	$31, $101
$100:
	.loc	2 1109

	.loc	2 1109

	subq	$16, $5, $16
	.loc	2 1109

$101:
	.loc	2 1109

	cmpult	$1, $17, $25
	bne	$25, $99
	subq	$17, $1, $27
	bne	$27, $102
	cmpult	$7, $16, $24
	bne	$24, $99
$102:
	.loc	2 1113
 # 1110		    }
 # 1111	
 # 1112		    /* Subtract ph:pl from rh:rl; we know rh will be 0 */
 # 1113		    rl -= pl;
	subq	$7, $16, $7
	.loc	2 1114
 # 1114		    *qq |= qa;
	ldq	$23, 0($10)
	or	$23, $3, $22
	stq	$22, 0($10)
	bne	$11, $92
$103:
	.loc	2 1118
 # 1115		}
 # 1116	
 # 1117		/* Denormalize dividend */
 # 1118		if (k != 0) {
	ldl	$25, 176($sp)
	beq	$25, $106
	.loc	2 1118

	.loc	2 1119
 # 1119			if((qq > nn) && (qq < &nn[orig_nl])) {
	cmpult	$12, $10, $27
	beq	$27, $104
	ldq	$19, 168($sp)
	s8addq	$19, $12, $24
	cmpult	$10, $24, $23
	beq	$23, $104
	.loc	2 1119

	.loc	2 1121
 # 1120				/* Overlap between qq and nn. Care of *qq! */
 # 1121				orig_nl = (qq - nn);
	.loc	2 1122
 # 1122				BnnShiftRight (nn, orig_nl, k);
	bis	$12, $12, $16
	subq	$10, $12, $17
	sra	$17, 3, $17
	bis	$25, $25, $18
	stq	$17, 88($sp)
	stq	$7, 152($sp)
	.livereg	0x0001E002,0x00000000
	jsr	$26, BnnShiftRight
	ldgp	$gp, 0($26)
	ldq	$17, 88($sp)
	ldq	$7, 152($sp)
	.loc	2 1123
 # 1123				nn[orig_nl - 1] = prev_qq;
	ldq	$22, 104($sp)
	addq	$17, -1, $27
	s8addq	$27, $12, $24
	stq	$22, 0($24)
	br	$31, $106
$104:
	ldq	$19, 168($sp)
	.loc	2 1124
 # 1124			} else if(qq == nn) {
	subq	$10, $12, $23
	bne	$23, $105
	.loc	2 1124

	.loc	2 1125
 # 1125				BnnShiftRight(&nn[orig_nl - 1], 1, k);
	addq	$19, -1, $25
	s8addq	$25, $12, $16
	ldiq	$17, 1
	ldl	$18, 176($sp)
	stq	$7, 152($sp)
	.livereg	0x0001E002,0x00000000
	jsr	$26, BnnShiftRight
	ldgp	$gp, 0($26)
	ldq	$7, 152($sp)
	br	$31, $106
$105:
	.loc	2 1126
 # 1126			} else {
	.loc	2 1127
 # 1127				BnnShiftRight (nn, orig_nl, k);
	bis	$12, $12, $16
	bis	$19, $19, $17
	ldl	$18, 176($sp)
	stq	$7, 152($sp)
	.livereg	0x0001E002,0x00000000
	jsr	$26, BnnShiftRight
	ldgp	$gp, 0($26)
	ldq	$7, 152($sp)
$106:
	.loc	2 1129
 # 1128		}	}
 # 1129		return (rl >> k);
	ldl	$27, 176($sp)
	srl	$7, $27, $0
	.livereg	0xFC7F0002,0x3FC00000
	ldq	$26, 0($sp)
	ldq	$9, 8($sp)
	ldq	$10, 16($sp)
	ldq	$11, 24($sp)
	ldq	$12, 32($sp)
	lda	$sp, 240($sp)
	ret	$31, ($26), 1
	.end	BnnDivideDigit
