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

open Lambda

type closure_entry =
  | Free_variable of int
  | Function of int

type closure_env =
  | Not_in_closure
  | In_closure of {
      entries: closure_entry Ident.tbl;
      env_pos: int;
    }

type compilation_env =
  { ce_stack: int Ident.tbl;
    ce_closure: closure_env }

type debug_event =
  { mutable ev_pos: int;                (* Position in bytecode *)
    ev_module: string;                  (* Name of defining module *)
    ev_loc: Location.t;                 (* Location in source file *)
    ev_kind: debug_event_kind;          (* Before/after event *)
    ev_defname: string;                 (* Enclosing definition *)
    ev_info: debug_event_info;          (* Extra information *)
    ev_typenv: Env.summary;             (* Typing environment *)
    ev_typsubst: Subst.t;               (* Substitution over types *)
    ev_compenv: compilation_env;        (* Compilation environment *)
    ev_stacksize: int;                  (* Size of stack frame *)
    ev_repr: debug_event_repr }         (* Position of the representative *)

and debug_event_kind =
    Event_before
  | Event_after of Types.type_expr
  | Event_pseudo

and debug_event_info =
    Event_function
  | Event_return of int
  | Event_other

and debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref

type closure_hint =
  { params : Lambda.value_kind list;
    return: Lambda.value_kind;
    inline : Lambda.inline_attribute;
    specialise : Lambda.specialise_attribute;
    is_a_functor : bool }

type ccall_hint =
  | Hint_unsafe
  | Hint_int of Primitive.boxed_integer
  | Hint_bigarray of
      { unsafe : bool;
        elt_kind : Lambda.bigarray_kind;
        layout : Lambda.bigarray_layout }
  | Hint_primitive of Primitive.description

type optimization_hint =
  | Hint_immutable_block
  | Hint_arraylength of Lambda.array_kind
  | Hint_closures of closure_hint list
  | Hint_ccall of ccall_hint

type label = int                     (* Symbolic code labels *)

type instruction =
    Klabel of label
  | Kacc of int
  | Kenvacc of int
  | Kpush
  | Kpop of int
  | Kassign of int
  | Kpush_retaddr of label
  | Kapply of int                       (* number of arguments *)
  | Kappterm of int * int               (* number of arguments, slot size *)
  | Kreturn of int                      (* slot size *)
  | Krestart
  | Kgrab of int                        (* number of arguments *)
  | Kclosure of label * int * closure_hint
  | Kclosurerec of (label * closure_hint) list * int
  | Koffsetclosure of int
  | Kgetglobal of Ident.t
  | Ksetglobal of Ident.t
  | Kconst of structured_constant
  | Kmakeblock of int * int * Asttypes.mutable_flag (* size, tag, mutable *)
  | Kmakefloatblock of int * Asttypes.mutable_flag
  | Kgetfield of int
  | Ksetfield of int
  | Kgetfloatfield of int
  | Ksetfloatfield of int
  | Kvectlength of Lambda.array_kind
  | Kgetvectitem
  | Ksetvectitem
  | Kgetstringchar
  | Kgetbyteschar
  | Ksetbyteschar
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstrictbranchif of label
  | Kstrictbranchifnot of label
  | Kswitch of label array * label array
  | Kboolnot
  | Kpushtrap of label
  | Kpoptrap
  | Kraise of raise_kind
  | Kcheck_signals
  | Kccall of string * int * ccall_hint option
  | Knegint | Kaddint | Ksubint | Kmulint | Kdivint | Kmodint
  | Kandint | Korint | Kxorint | Klslint | Klsrint | Kasrint
  | Kintcomp of integer_comparison
  | Koffsetint of int
  | Koffsetref of int
  | Kisint
  | Kisout
  | Kgetmethod
  | Kgetpubmet of int
  | Kgetdynmet
  | Kevent of debug_event
  | Kperform
  | Kresume
  | Kresumeterm of int
  | Kreperformterm of int
  | Kstop

let immed_min = -0x40000000
and immed_max = 0x3FFFFFFF

(* Actually the abstract machine accommodates -0x80000000 to 0x7FFFFFFF,
   but these numbers overflow the OCaml type int if the compiler runs on
   a 32-bit processor. *)
