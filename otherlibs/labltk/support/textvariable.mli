(* $Id$ *)

(* Support for Tk -textvariable option *)
open Widget
open Protocol

type textVariable
      (* TextVariable is an abstract type *)

val create : ?on: 'a widget -> unit -> textVariable
      (* Allocation of a textVariable with lifetime associated to widget 
	 if a widget is specified *)
val set : textVariable -> to: string -> unit
      (* Setting the val of a textVariable *)
val get : textVariable -> string
      (* Reading the val of a textVariable *)
val name : textVariable -> string
      (* Its tcl name *)

val cCAMLtoTKtextVariable : textVariable -> tkArgs
      (* Internal conversion function *)

val handle : textVariable -> (unit -> unit) -> unit
      (* Callbacks on variable modifications *)

val coerce : string -> textVariable

(*-*)
val free : textVariable -> unit
