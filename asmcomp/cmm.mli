(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Second intermediate language (machine independent) *)

type machtype_component =
  | Val
  | Addr
  | Int
  | Float

(* - [Val] denotes a valid OCaml value: either a pointer to the beginning
     of a heap block, an infix pointer if it is preceded by the correct
     infix header, or a 2n+1 encoded integer.
   - [Int] is for integers (not necessarily 2n+1 encoded) and for
     pointers outside the heap.
   - [Addr] denotes pointers that are neither [Val] nor [Int], i.e.
     pointers into the heap that point in the middle of a heap block.
     Such derived pointers are produced by e.g. array indexing.
   - [Float] is for unboxed floating-point numbers.

The purpose of these types is twofold.  First, they guide register
allocation: type [Float] goes in FP registers, the other types go
into integer registers.  Second, they determine how local variables are 
tracked by the GC:
   - Variables of type [Val] are GC roots.  If they are pointers, the
     GC will not deallocate the addressed heap block, and will update
     the local variable if the heap block moves.
   - Variables of type [Int] and [Float] are ignored by the GC.
     The GC does not change their values.
   - Variables of type [Addr] must never be live across an allocation
     point or function call.  They cannot be given as roots to the GC
     because they don't point after a well-formed block header of the
     kind that the GC needs.  However, the GC may move the block pointed
     into, invalidating the value of the [Addr] variable.
*)

type machtype = machtype_component array

val typ_void: machtype
val typ_val: machtype
val typ_addr: machtype
val typ_int: machtype
val typ_float: machtype

val size_component: machtype_component -> int
val size_machtype: machtype -> int

type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge

val negate_comparison: comparison -> comparison
val swap_comparison: comparison -> comparison

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int                           (* integer or pointer outside heap *)
  | Word_val                           (* pointer inside heap or encoded int *)
  | Single
  | Double                             (* 64-bit-aligned 64-bit float *)
  | Double_u                           (* word-aligned 64-bit float *)

type operation =
    Capply of machtype * Debuginfo.t
  | Cextcall of string * machtype * bool * Debuginfo.t
  | Cload of memory_chunk
  | Calloc
  | Cstore of memory_chunk
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of comparison
  | Caddv (* pointer addition that produces a [Val] (well-formed Caml value) *)
  | Cadda (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa of comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of comparison
  | Craise of Lambda.raise_kind * Debuginfo.t
  | Ccheckbound of Debuginfo.t

type expression =
    Cconst_int of int
  | Cconst_natint of nativeint
  | Cconst_float of float
  | Cconst_symbol of string
  | Cconst_pointer of int
  | Cconst_natpointer of nativeint
  | Cconst_blockheader of nativeint
  | Cvar of Ident.t
  | Clet of Ident.t * expression * expression
  | Cassign of Ident.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array
  | Cloop of expression
  | Ccatch of int * Ident.t list * expression * expression
  | Cexit of int * expression list
  | Ctrywith of expression * Ident.t * expression

type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression;
    fun_fast: bool;
    fun_dbg : Debuginfo.t; }

type data_item =
    Cdefine_symbol of string
  | Cdefine_label of int
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Clabel_address of int
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list
