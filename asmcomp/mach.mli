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

(* Representation of machine code by sequences of pseudoinstructions *)

(** N.B. Backends vary in their treatment of call gc and checkbound
    points.  If the positioning of any labels associated with these is
    important for some new feature in the compiler, the relevant backends'
    behaviour should be checked. *)
type label = Cmm.label

type integer_comparison =
    Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound of { label_after_error : label option;
        spacetime_index : int; }
    (** For Spacetime only, [Icheckbound] operations take two arguments, the
        second being the pointer to the trie node for the current function
        (and the first being as per non-Spacetime mode). *)

type float_comparison = Cmm.float_comparison

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_comparison
  | Ioddtest
  | Ieventest

type phantom_defining_expr =
  | Iphantom_const_int of Targetint.t
  | Iphantom_const_symbol of Backend_sym.t
  | Iphantom_var of Backend_var.t
  | Iphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  | Iphantom_read_field of { var : Backend_var.t; field : int; }
  | Iphantom_read_symbol_field of { sym : Backend_sym.t; field : int; }
  | Iphantom_block of { tag : int; fields : Backend_var.t option list; }

val phantom_defining_expr_definitely_static : phantom_defining_expr -> bool

type call_labels = {
  before : label;
  after : label;
}

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of int64
  | Iconst_symbol of Backend_sym.t
  | Icall_ind of { call_labels : call_labels; }
  | Icall_imm of {
      func : Backend_sym.t;
      callee_dbg : Debuginfo.Function.t option;
      call_labels : call_labels;
    }
  | Itailcall_ind of { call_labels : call_labels; }
  | Itailcall_imm of {
      func : Backend_sym.t;
      callee_dbg : Debuginfo.Function.t option;
      call_labels : call_labels;
    }
  | Iextcall of { func : Backend_sym.t; alloc : bool;
      call_labels : call_labels; }
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
                                 (* false = initialization, true = assignment *)
  | Ialloc of { bytes : int; label_after_call_gc : label option;
      spacetime_index : int; }
    (** For Spacetime only, Ialloc instructions take one argument, being the
        pointer to the trie node for the current function. *)
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Backend_var.t;
      is_parameter : Is_parameter.t;
      provenance : Backend_var.Provenance.t option; is_assignment : bool; }
    (** [Iname_for_debugger] has the following semantics:
        (a) The argument register(s) is/are deemed to contain the value of the
            given identifier.
        (b) If [is_assignment] is [true], any information about other [Reg.t]s
            that have been previously deemed to hold the value of that
            identifier is forgotten. *)

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    mutable live: Reg.Set.t;
    mutable dbg: Insn_debuginfo.t;
  }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Icatch of Cmm.rec_flag * (int * instruction) list * instruction
  | Iexit of int
  | Itrywith of instruction * instruction
  | Iraise of Cmm.raise_kind

type spacetime_part_of_shape =
  | Direct_call_point of { callee : Backend_sym.t; }
  | Indirect_call_point
  | Allocation_point

(** A description of the layout of a Spacetime profiling node associated with
    a given function.  Each call and allocation point instrumented within
    the function is marked with a label in the code and assigned a place
    within the node.  This information is stored within the executable and
    extracted when the user saves a profile.  The aim is to minimise runtime
    memory usage within the nodes and increase performance. *)
type spacetime_shape = (spacetime_part_of_shape * Cmm.label) list

type fundecl =
  { fun_name: Backend_sym.t;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.Function.t;
    fun_spacetime_shape : spacetime_shape option;
    fun_phantom_lets :
      (Backend_var.Provenance.t option * phantom_defining_expr)
        Backend_var.Map.t;
  }

type phantom_available_before =
  | Take_from of instruction
  | Exactly of Backend_var.Set.t

val dummy_instr: instruction
val end_instr: unit -> instruction

val instr_cons_debug
   : phantom_available_before:phantom_available_before
  -> instruction_desc
  -> Reg.t array
  -> Reg.t array
  -> Debuginfo.t
  -> instruction
  -> instruction

(** Cons an instruction onto the given [next] instruction.  The new instruction
    has the specified [instruction_desc] and an empty [live] field.  The
    [arg], [res] and [dbg] fields of the new instruction are those of [from]
    except where overridden by one or more of the optional arguments. *)
val instr_cons_from
   : ?arg:Reg.t array
  -> ?res:Reg.t array
  -> ?dbg:Insn_debuginfo.t
  -> instruction
  -> instruction_desc
  -> next:instruction
  -> instruction

val instr_iter: (instruction -> unit) -> instruction -> unit

val spacetime_node_hole_pointer_is_live_before : instruction -> bool

val operation_can_raise : operation -> bool
