/* Copyright     Digital Equipment Corporation & INRIA     1988, 1989 */
/* Last modified_on Tue Feb 25  1:27:57 GMT+1:00 1992 by shand */
/*      modified_on Mon Apr 15 18:44:14 GMT+2:00 1991 by herve */

#include <stdio.h>
#include "BigZ.h"

#ifndef MSDOS
#define S(A,B)		strcmp(A,B)
#define P(A)		fprintf(stderr,"%d...",A)
#define E(A,B,C)	fprintf(stderr,"\nError in test #%d:\nComputed: %s\nCorrect:  %s\n",A,C,B)
#define T(A,B,C)	S(B,C)?E(A,B,C):P(A)
#else
void T(A,B,C)
int A;
char *B, *C;
{
    if (strcmp (B, C))
	    fprintf (stderr, "\nError in test #%d:\nComputed: %s\nCorrect:  %s\n",A,C,B);
    else
	    fprintf (stderr,"%2d...",A);
}
#endif
#define NEWLINE		fprintf(stderr,"\n")
#define To(A)		BzToString(A,10)
#define From(A)		BzFromString(A,10)
#define Abs(A)		BzAbs(A)
#define Neg(A)		BzNegate(A)
#define Add(A,B)	BzAdd(A,B)
#define Sub(A,B)	BzSubtract(A,B)
#define Mul(A,B)	BzMultiply(A,B)
#define Div(A,B)	BzDiv(A,B)
#define Mod(A,B)	BzMod(A,B)
#define Fac(A)		BzFactorial(A)
#define FromI(I)        BzFromInteger(I)
#define Cmp(A,B)	BzCompare(A,B)
#define Sqa(A)          Mul(A,A)

#define zero		FromI(0)
#define one             FromI(1)
#define two             FromI(2)
#define minusone        FromI(-1)

#ifdef DIGITonUSHORT
#define two31m1         Sub(Mul(From("65536"),From("32768")),one)
#else
#define two31m1         FromI(0x7FFFFFFF)
#endif

main()
{
   BigZ a,b;

   T(1,"12", To(From("12"))) ;
   T(2,"12345678910", To(From("12345678910"))) ;
   T(3,"123", To(From("00000123"))) ;
   T(4,"-123", To(From("-123"))) ;
   T(5,"-32768", To(From("-32768"))) ;
   T(6,"-32768", To(Neg(From("32768")))) ;
   T(7,"-32768", To(Add(From("-16384"),From("-16384")))) ;
   T(8,"-32768", To(Add(From("-16383"),From("-16385")))) ;
   T(9,"-32768", To(Mul(From("2"),From("-16384")))) ;
   T(10,"-16384", To(Div(From("-32768"),From("2")))) ;
   NEWLINE;
   T(11,"100000", To(Add(From("1"),From("99999")))) ;
   T(12,"12343994",To(Add(From("-1684"),From("12345678"))));
   T(13,"-12329294",To(Sub(From("16384"),From("12345678"))));
   T(14,"135801",To(Add(From("12345"),From("123456"))));
   T(15,"123456135801",To(Add(From("12345"),From("123456123456"))));
   T(16,"135801",To(Add(From("123456"),From("12345"))));
   T(17,"123456135801",To(Add(From("123456123456"),From("12345"))));
   T(18,"135801",To(Sub(From("12345"),From("-123456"))));
   T(19,"123456135801",To(Sub(From("12345"),From("-123456123456"))));
   T(20,"135801",To(Sub(From("123456"),From("-12345"))));
   NEWLINE;
   T(21,"123456135801",To(Sub(From("123456123456"),From("-12345"))));
   T(22,"-111111",To(Sub(From("12345"),From("123456"))));
   T(23,"111111",To(Sub(From("123456"),From("12345"))));
   T(24,"-123456111111",To(Sub(From("12345"),From("123456123456"))));
   T(25,"123456111111",To(Sub(From("123456123456"),From("12345"))));
   T(26,"-111111",To(Add(From("12345"),From("-123456"))));
   T(27,"111111",To(Add(From("123456"),From("-12345"))));
   T(28,"-123456111111",To(Add(From("12345"),From("-123456123456"))));
   T(29,"123456111111",To(Add(From("123456123456"),From("-12345"))));
   T(30,"2", To(Div(From("264195"),From("97200")))) ;
   NEWLINE;
   T(31,"27405", To(Mod(From("97200"),From("69795")))) ;
   T(32,"4294967295", To(Div(From("22685491128062564230891640495451214097"),From("5281877500950955845296219748")))) ;
   T(33,"99997",To(Add(From("-3"),From("100000"))));
   T(34,"-100003",To(Add(From("-3"),From("-100000"))));
   T(35,"999999",To(Sub(From("1000000"),From("1"))));
   T(36,"999999999",To(Mul(From("12345679"),From("81"))));
   a = From("1234567");
   b = From("123456");
   T(37,"1234567",To(Add(Mul(Div(a,b),b),Mod(a,b))));
   T(38,"-1234567",To(Add(Mul(Div(Neg(a),Neg(b)),Neg(b)),Mod(Neg(a),Neg(b)))));
   T(39,"1234567",To(Add(Mul(Div(a,Neg(b)),Neg(b)),Mod(a,Neg(b)))));
   T(40,"10000000000000000000000",To(Mul(From("-100000000000"),From("-100000000000"))));
   NEWLINE;
   T(41,"-10000000000000000000000",To(Mul(From("-100000000000"),From("100000000000"))));
   T(42,"-10000000000000000000000",To(Mul(From("100000000000"),From("-100000000000"))));
   T(43,"10000000000000000000000",To(Mul(From("100000000000"),From("100000000000"))));
   a = Sub(From("10000000000000"),From("10000000000000"));
   T(44,"0",To(Mod(a,From("1000000000000"))));
   T(45,"0",To(Div(a,From("1000000000000"))));
   T(46,"0",To(Mod(Neg(a),From("10000000000000"))));
   T(47,"0",To(Div(Neg(a),From("10000000000000"))));
   T(48,"2",To(Div(From("3000"),Sub(From("1234567891234"),From("1234567890000")))));
   T(49,"532",To(Mod(From("3000"),Sub(From("1234567891234"),From("1234567890000")))));
   T(50,"9",To(Mod(From("-1234567890"),From("1234567899"))));
   NEWLINE;
   T(51,"2",To(Mod(Sub(From("12345678900000"),From("12345678926887")),From("3"))));
   T(52,"40830949904677684825316369628906250000000000000",To(Mul(From("48270948888581289062500000000"),From("845870049062500000"))));
   T(53,"22666179639240748063923391983020279316955515",To(Mul(From("6956883693"),From("3258093801689886619170103176686855"))));
   T(54,"1405006117752879898543142606244511569936384000000000",To(Fac(From("42"))));
   T(55,"0",To(Mod(Fac(From("13")),Fac(From("9")))));
   T(56,"0",To(Mod(Fac(From("34")),Fac(From("13")))));
   T(57,"0",To(Mod(Fac(From("57")),Fac(From("21")))));
   T(58,"0",To(Mod(Fac(From("40")),Fac(From("39")))));
   T(59,"59",To(Div(Fac(From("59")),Fac(From("58")))));
   T(60,"2",To(Div(From("5"),From("2"))));
   NEWLINE;
   T(61,"1",To(Mod(From("5"),From("2"))));
   T(62,"-3",To(Div(From("-5"),From("2"))));
   T(63,"1",To(Mod(From("-5"),From("2"))));
   T(64,"3",To(Div(From("-5"),From("-2"))));
   T(65,"1",To(Mod(From("-5"),From("-2"))));
   T(66,"-2",To(Div(From("5"),From("-2"))));
   T(67,"1",To(Mod(From("5"),From("-2"))));
   T(68,"3",To(Div(From("6"),From("2"))));
   T(69,"0",To(Mod(From("6"),From("2"))));
   T(70,"-3",To(Div(From("-6"),From("2"))));
   NEWLINE;
   T(71,"0",To(Mod(From("-6"),From("2"))));
   T(72,"3",To(Div(From("-6"),From("-2"))));
   T(73,"0",To(Mod(From("-6"),From("-2"))));
   T(74,"-3",To(Div(From("6"),From("-2"))));
   T(75,"0",To(Mod(From("6"),From("-2"))));
   T(76,"0",To(Abs(From("0"))));
   T(77,"1234567890",To(Abs(From("1234567890"))));
   T(78,"1234567890",To(Abs(From("-1234567890"))));
   T(79,"1",BzCompare(From("-1234567890"),From("12345"))<0?"1":"0");
   T(80,"1",BzGetSign(From("-1234567890"))<0?"1":"0");
   NEWLINE; 
   T(81,"0", To(Add(From("-1"),Mul(From("-1"),From("-1")))));
   T(82,"-1",To(Add(From("-1"),Mul(From("0"), From("-1")))));
   T(83,"-3",To(Add(From("-1"),Mul(From("-2"),From("1" )))));
   T(84,"1", To(Add(From("-1"),Mul(From("-2"),From("-1")))));
   T(85,"-1",To(Add(From("1"), Mul(From("-2"),From("1" )))));
   T(86,"18446744065119617025",To(Mul(From("4294967295"),From("4294967295"))));
        /* (-2^64 + 2^32 - 1) / 2^32 */
   T(87,"-4294967296",To(Div(
        Sub(Mul(Mul(Add(Mul(two31m1,two),one),Mul(Add(two31m1,one), two)),minusone),one),
        Mul(Add (two31m1,one),two))));
   T(88,"Equal",(Cmp(Mod(FromI(10),FromI(5)),zero) == BZ_EQ)?"Equal":"Not equal");
   T(89,"Equal",(Cmp(Div(FromI(4),FromI(5)),zero) == BZ_EQ)?"Equal":"Not equal");
   a = From ("100000000000000000000000000000000000000");
   T(90,To (a),To(Div (Sqa (a),a)));
	/* 90: 	tests the MIPS & turbo C optimizer bugs. If the special */
	/*     	purpose squaring code is enabled and the optimizer      */
	/*	messes up, this test will fail                          */
   NEWLINE;
   b = Sqa (a);
   T(91,To (b),To(Div (Sqa (b),b)));
   T(92,"-1",To(Div(From("13"),From("-13"))));
   NEWLINE;
}
