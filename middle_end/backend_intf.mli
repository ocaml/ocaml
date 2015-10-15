(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Knowledge that the middle end needs about the backend. *)

module type S = sig
  (* CR mshinwell for pchambart: Can you define exactly what makes an
     [Ident.t] "global"? *)
  val symbol_for_global' : (Ident.t -> Symbol.t)

  (* If the given approximation is that of a symbol (Value_symbol) or an
     external (Value_extern), attempt to find a more informative
     approximation from a previously-written compilation artifact.  In the
     native code backend, for example, this might consult a .cmx file. *)
  val really_import_approx : Simple_value_approx.t -> Simple_value_approx.t

  (* Find the approximation for the given global identifier. *)
  val import_global : Ident.t -> Simple_value_approx.t

  (* CR mshinwell for pchambart: At first sight it looks like
     [import_global] might be written as

        let import_global sym = import_symbol (symbol_for_global' sym)

     but I'm not sure that's true, looking at the code in
     asmcomp/import_approx.ml.  Can you elaborate?
  *)
  val import_symbol : Symbol.t -> Simple_value_approx.t

  val closure_symbol : Closure_id.t -> Symbol.t

  (* The natural size of an integer on the target architecture
     (cf. [Arch.size_int] in the native code backend). *)
  val size_int : int

  (* [true] iff the target architecture is big endian. *)
  val big_endian : bool
end
