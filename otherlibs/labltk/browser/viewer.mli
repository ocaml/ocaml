(* $Id$ *)

(* Module viewer *)
open Widget

val search_symbol : unit -> unit
      	(* search a symbol in all modules in the path *)

val f : ?dir:string -> ?on:toplevel widget -> unit -> unit
      	(* open then module viewer *)

val view_defined : Longident.t -> env:Env.t -> unit
      	(* displays a signature, found in environment *)

val close_all_views : unit -> unit
