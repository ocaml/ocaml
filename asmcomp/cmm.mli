(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Second intermediate language (machine independent) *)

type machtype_component =
  | Val
  | Int
  | Float
  | Derived_val
  | Arbitrary_out_of_heap
(* - [Val] denotes a valid OCaml value: either a pointer to the beginning
     of a heap block, an infix pointer if it is preceded by the correct
     infix header, or a 2n+1 encoded integer.
   - [Int] is for integers (not necessarily 2n+1 encoded) or out-of-heap
     pointers to valid OCaml values (which cannot, themselves or transitively,
     point back into the heap).
   - [Float] is for unboxed floating-point numbers.
   - [Derived_val] denotes pointers produced from in-heap [Val] pointers that
     may point into the middle of heap blocks.  (Example: pointers produced
     by array indexing.)
   - [Arbitrary_out_of_heap] denotes an out-of-heap pointer (for example a
     pointer to the code of a function) that cannot be scanned by the GC.

   The purpose of these types is twofold.  First, they guide register
   allocation: type [Float] goes in FP registers, the other types go
   into integer registers.  Second, they determine how local variables are
   tracked by the GC:
   - Variables of type [Val] are GC roots.  If they are pointers, the
     GC will not deallocate the addressed heap block, and will update
     the local variable if the heap block moves.
   - Variables of type [Int] are not usually registered as GC roots but may be
     on occasion, for example following a conditional where the other arm
     returns a [Val].  (Hence the requirement, as above, that out-of-heap
     pointers of type [Int] must point at blocks that are laid out in the same
     way as OCaml values.)
   - Variables of type [Float] and variables of type [Arbitrary_out_of_heap]
     are never registered as GC roots.  The GC neither examines such variables
     nor changes their values.
   - Variables of type [Derived_val] must never be live across an allocation
     point or function call.  They cannot be given as roots to the GC
     because they don't point after a well-formed block header of the
     kind that the GC needs.  However, the GC may move the block pointed
     into, invalidating the value of the [Derived_val] variable.
*)

type machtype = machtype_component array

val typ_void: machtype
val typ_val: machtype
val typ_derived_val: machtype
val typ_int: machtype
val typ_float: machtype
val typ_arbitrary_out_of_heap: machtype

val size_component: machtype_component -> int

(** Least upper bound of two [machtype_component]s. *)
val lub_component
   : machtype_component
  -> machtype_component
  -> machtype_component

(** Returns [true] iff the first supplied [machtype_component] is greater than
    or equal to the second under the relation used by [lub_component]. *)
val ge_component
   : machtype_component
  -> machtype_component
  -> bool

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

type label = int
val new_label: unit -> label

type raise_kind =
  | Raise_withtrace
  | Raise_notrace

type rec_flag = Nonrecursive | Recursive

type symbol_type =
  | Function
  (** A code pointer. *)
  | Value
  (** An out-of-heap value that may be scanned (but of course does not need
      to be scanned) by the GC. *)
  | Other
  (** An out-of-heap value that cannot be scanned by the GC. *)

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int                           (* integer *)
  | Word_out_of_heap of symbol_type    (* pointer outside heap *)
  | Word_val                           (* pointer inside heap or encoded int *)
  | Single
  | Double                             (* 64-bit-aligned 64-bit float *)
  | Double_u                           (* word-aligned 64-bit float *)

and operation =
    Capply of machtype
  | Cextcall of string * machtype * bool * label option
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc
  | Cstore of memory_chunk * Lambda.initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of comparison
  | Caddv (* pointer addition that produces a [Val] (well-formed Caml value) *)
  | Cadda (* ditto that produces a [Derived_val] (derived heap pointer) *)
  | Caddov (* ditto that produces an [Out_of_heap_val] *)
  | Ccmpa of comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of comparison
  | Craise of raise_kind
  | Ccheckbound

(** Not all cmm expressions currently have [Debuginfo.t] values attached to
    them.  The ones that do are those that are likely to generate code that
    can fairly robustly be mapped back to a source location.  In the future
    it might be the case that more [Debuginfo.t] annotations are desirable. *)
and expression =
    Cconst_int of int
  | Cconst_natint of nativeint
  | Cconst_float of float
  | Cconst_symbol of string * symbol_type
  | Cconst_pointer of int
  | Cconst_natpointer of nativeint
  | Cblockheader of nativeint * Debuginfo.t
  | Cvar of Ident.t
  | Clet of Ident.t * expression * expression
  | Cassign of Ident.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array * Debuginfo.t
  | Cloop of expression
  | Ccatch of rec_flag * (int * Ident.t list * expression) list * expression
  | Cexit of int * expression list
  | Ctrywith of expression * Ident.t * expression

type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of string
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

val ccatch : int * Ident.t list * expression * expression -> expression

val reset : unit -> unit
