(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

class reload_generic (unit) =
  method reload_operation :
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array
  method reload_test : Mach.test -> Reg.t array -> Reg.t array
    (* Can be overriden to reflect instructions that can operate
       directly on stack locations *)
  method makereg : Reg.t -> Reg.t
    (* Can be overriden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
  method fundecl : Mach.fundecl -> Mach.fundecl * bool
    (* The entry point *)
end
