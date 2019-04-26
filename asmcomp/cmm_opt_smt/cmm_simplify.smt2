;**************************************************************************
;*                                                                        *
;*                                 OCaml                                  *
;*                                                                        *
;*                  Stefan Muenzel, Jane Street Asia                      *
;*                                                                        *
;*   Copyright 2019 Jane Street Group LLC                                 *
;*                                                                        *
;*   All rights reserved.  This file is distributed under the terms of    *
;*   the GNU Lesser General Public License version 2.1, with the          *
;*   special exception on linking described in the file LICENSE.          *
;*                                                                        *
;**************************************************************************


(define-sort ocaml-int () (_ BitVec 64))

(declare-datatypes () ((CMP EQ NE LT GT LE GE)))

(declare-datatypes () ((ILOG AND OR XOR)))

(define-fun ocaml-lsl ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvshl x y))

(define-fun ocaml-lsr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvlshr x y))

(define-fun ocaml-or ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvor x y))

(define-fun ocaml-and ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvand x y))

(define-fun ocaml-xor ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvxor x y))

(define-fun ocaml-asr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvashr x y))

(define-fun ocaml-has-tag ((x ocaml-int)) bool
  (= (ocaml-and x #x0000000000000001) #x0000000000000001))

(define-fun ocaml-highest-bits-same ((x ocaml-int)) bool
  (= ((_ extract 63 63) x) ((_ extract 62 62) x)))

(define-fun ocaml-lsr-1 ((x ocaml-int)) ocaml-int
  (ocaml-lsr x #x0000000000000001))

(define-fun ocaml-addi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvadd x y))

(define-fun ocaml-subi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsub x y))

(define-fun ocaml-tagi ((x ocaml-int)) ocaml-int
  (bvadd (ocaml-lsl x #x0000000000000001) #x0000000000000001))

(define-fun ocaml-untagi ((x ocaml-int)) ocaml-int
  (ocaml-asr x #x0000000000000001))

(define-fun ocaml-to-int-untagged ((x ocaml-int)) Int
  (+ (if (= ((_ extract 63 63) x) #b1) -16 0)
  (bv2int ((_ extract 62 0) x))))

(define-fun ocaml-cmp-eq ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (= x y) #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp-ne ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (not (= x y)) #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp-lt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvslt x y)
    #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp-gt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsgt x y)
    #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp-le ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsle x y)
    #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp-ge ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsge x y)
    #x0000000000000001 #x0000000000000000))

(define-fun ocaml-cmp ((cmp CMP) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match cmp
    (case EQ (ocaml-cmp-eq x y))
    (case NE (ocaml-cmp-ne x y))
    (case LT (ocaml-cmp-lt x y))
    (case GT (ocaml-cmp-gt x y))
    (case LE (ocaml-cmp-le x y))
    (case GE (ocaml-cmp-ge x y))
    ))

(define-fun ocaml-ilog ((ilog ILOG) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match ilog
    (case AND (ocaml-and x y))
    (case OR  (ocaml-or x y))
    (case XOR (ocaml-xor x y))
  ))



(push 1)
(echo "verifying nested-compare")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(declare-const cmp_outer CMP)
(declare-const cmp_inner CMP)
(assert-not
 (=
  (ocaml-cmp cmp_outer
   (ocaml-addi
    (ocaml-lsl (ocaml-cmp cmp_inner left right)
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   #b0000000000000000000000000000000000000000000000000000000000000001)
  (ocaml-cmp cmp_outer (ocaml-cmp cmp_inner left right)
   #b0000000000000000000000000000000000000000000000000000000000000000)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying nested-equal-one")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(declare-const cmp_inner CMP)
(assert-not
 (=
  (ocaml-cmp EQ (ocaml-cmp cmp_inner left right)
   #b0000000000000000000000000000000000000000000000000000000000000001)
  (ocaml-cmp cmp_inner left right)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying nested-equal-three-tagged")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(declare-const cmp_inner CMP)
(assert-not
 (=
  (ocaml-cmp EQ
   (ocaml-addi
    (ocaml-lsl (ocaml-cmp cmp_inner left right)
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   #b0000000000000000000000000000000000000000000000000000000000000011)
  (ocaml-cmp cmp_inner left right)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying nested-nequal-zero")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(declare-const cmp_inner CMP)
(assert-not
 (=
  (ocaml-cmp NE (ocaml-cmp cmp_inner left right)
   #b0000000000000000000000000000000000000000000000000000000000000000)
  (ocaml-cmp cmp_inner left right)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying tag-and")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(assert-not
 (=
  (ocaml-and
   (ocaml-addi
    (ocaml-lsl left
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   (ocaml-addi
    (ocaml-lsl right
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001))
  (ocaml-addi
   (ocaml-lsl (ocaml-and left right)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   #b0000000000000000000000000000000000000000000000000000000000000001)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying tag-or")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(assert-not
 (=
  (ocaml-or
   (ocaml-addi
    (ocaml-lsl left
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   (ocaml-addi
    (ocaml-lsl right
     #b0000000000000000000000000000000000000000000000000000000000000001)
    #b0000000000000000000000000000000000000000000000000000000000000001))
  (ocaml-addi
   (ocaml-lsl (ocaml-or left right)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   #b0000000000000000000000000000000000000000000000000000000000000001)))
(check-sat)
(pop 1)


(push 1)
(echo "verifying tag-xor")
(declare-const right ocaml-int)
(declare-const left ocaml-int)
(assert-not
 (=
  (ocaml-or
   (ocaml-xor
    (ocaml-addi
     (ocaml-lsl left
      #b0000000000000000000000000000000000000000000000000000000000000001)
     #b0000000000000000000000000000000000000000000000000000000000000001)
    (ocaml-addi
     (ocaml-lsl right
      #b0000000000000000000000000000000000000000000000000000000000000001)
     #b0000000000000000000000000000000000000000000000000000000000000001))
   #b0000000000000000000000000000000000000000000000000000000000000001)
  (ocaml-addi
   (ocaml-lsl (ocaml-xor left right)
    #b0000000000000000000000000000000000000000000000000000000000000001)
   #b0000000000000000000000000000000000000000000000000000000000000001)))
(check-sat)
(pop 1)
