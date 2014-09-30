(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers for Intel code generators *)

(* The DSL* modules expose functions to emit x86/x86_64 instructions
   using a syntax close to AT&T (in particular, arguments are reversed compared
   to the official Intel syntax).

   Some notes:

     - Unary floating point instructions such as fadd/fmul/fstp/fld/etc come with a single version
       supporting both the single and double precision instructions.  (As with Intel syntax.)

     - A legacy bug in GAS:
       https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
       is not replicated here.  It is managed by Intel_gas.
*)


open Intel_ast

module DSL : sig
  val sym: string -> arg
  val emit_nat: nativeint -> arg
  val int: int -> arg
  val const_64: int64 -> constant
  val const_32: int32 -> constant
  val const_nat: nativeint -> constant
  val const: int -> constant
  val _cfi_startproc: unit -> unit
  val _cfi_endproc: unit -> unit
  val _cfi_adjust_cfa_offset: int -> unit
  val _file: int -> string -> unit
  val _loc: int -> int -> unit
  val _text: unit -> unit
  val _data: unit -> unit
  val _section: string list -> string option -> string list -> unit
  val _386: unit -> unit
  val _model: string -> unit
  val _global: string -> unit
  val _align: int -> unit
  val _llabel: ?typ:data_type -> string -> unit
  val _comment: string -> unit
  val _extrn: string -> data_type -> unit
  val _private_extern: string -> unit
  val _indirect_symbol: string -> unit
  val _size: string -> constant -> unit
  val _type: string -> string -> unit
  val _qword: constant -> unit
  val _long: constant -> unit
  val _word: constant -> unit
  val _byte: constant -> unit
  val _ascii: string -> unit
  val _space: int -> unit
  val _setvar: string * constant -> unit
  val _end: unit -> unit
  val al: arg
  val ah: arg
  val cl: arg
  val ax: arg
  val rax: arg
  val r10: arg
  val r11: arg
  val r14: arg
  val r15: arg
  val rsp: arg
  val rbp: arg
  val xmm15: arg
  val eax: arg
  val ebx: arg
  val ecx: arg
  val edx: arg
  val ebp: arg
  val esp: arg
  val st0: arg
  val st1: arg
end
module I : sig
  val add: arg * arg -> unit
  val addsd: arg * arg -> unit
  val and_: arg * arg -> unit
  val andpd: arg * arg -> unit
  val bswap: arg -> unit
  val call: arg -> unit
  val cdq: unit -> unit
  val cmp: arg * arg -> unit
  val comisd: arg * arg -> unit
  val cqo: unit -> unit
  val cvtsd2ss: arg * arg -> unit
  val cvtsi2sd: arg * arg -> unit
  val cvtss2sd: arg * arg -> unit
  val cvttsd2si: arg * arg -> unit
  val dec: arg -> unit
  val divsd: arg * arg -> unit
  val fabs: unit -> unit
  val fadd: arg -> unit
  val faddp: arg * arg -> unit
  val fchs: unit -> unit
  val fcomp: arg -> unit
  val fcompp: unit -> unit
  val fcos: unit -> unit
  val fdiv: arg -> unit
  val fdivp: arg * arg -> unit
  val fdivr: arg -> unit
  val fdivrp: arg * arg -> unit
  val fild: arg -> unit
  val fistp: arg -> unit
  val fld1: unit -> unit
  val fld: arg -> unit
  val fldcw: arg -> unit
  val fldlg2: unit -> unit
  val fldln2: unit -> unit
  val fldz: unit -> unit
  val fmul: arg -> unit
  val fmulp: arg * arg -> unit
  val fnstcw: arg -> unit
  val fnstsw: arg -> unit
  val fpatan: unit -> unit
  val fptan: unit -> unit
  val fsin: unit -> unit
  val fsqrt: unit -> unit
  val fstp: arg -> unit
  val fsub: arg -> unit
  val fsubp: arg * arg -> unit
  val fsubr: arg -> unit
  val fsubrp: arg * arg -> unit
  val fxch: arg -> unit
  val fyl2x: unit -> unit
  val hlt: unit -> unit
  val idiv: arg -> unit
  val imul: arg * arg option -> unit
  val inc: arg -> unit
  val j: condition -> arg -> unit
  val ja: arg -> unit
  val jae: arg -> unit
  val jb: arg -> unit
  val jbe: arg -> unit
  val je: arg -> unit
  val jg: arg -> unit
  val jmp: arg -> unit
  val jne: arg -> unit
  val jp: arg -> unit
  val lea: arg * arg -> unit
  val mov: arg * arg -> unit
  val movapd: arg * arg -> unit
  val movsd: arg * arg -> unit
  val movss: arg * arg -> unit
  val movsx: arg * arg -> unit
  val movsxd: arg * arg -> unit
  val movzx: arg * arg -> unit
  val mulsd: arg * arg -> unit
  val nop: unit -> unit
  val or_: arg * arg -> unit
  val pop: arg -> unit
  val push: arg -> unit
  val ret: unit -> unit
  val sal: arg * arg -> unit
  val sar: arg * arg -> unit
  val set: condition -> arg -> unit
  val shr: arg * arg -> unit
  val sqrtsd: arg * arg -> unit
  val sub: arg * arg -> unit
  val subsd: arg * arg -> unit
  val test: arg * arg -> unit
  val ucomisd: arg * arg -> unit
  val xchg: arg * arg -> unit
  val xor: arg * arg -> unit
  val xorpd: arg * arg -> unit
end
module DSL32 : sig
  include module type of DSL

  val mem32:
    data_type -> ?scale:int -> ?base:reg64 -> ?sym:string ->
    int -> reg64 -> arg
  val mem_sym: data_type -> ?ofs:int -> string -> arg
end
module DSL64 : sig
  include module type of DSL

  val mem64:
    data_type -> ?scale:int -> ?base:reg64 -> ?sym:string ->
    int -> reg64 -> arg
  val from_rip: data_type -> ?ofs:int -> string -> arg
end
