(* $Id$ *)

open Widget
open Mytypes

val nowarnings : bool ref

val f : edit_window -> any widget list
      	(* Typechecks the window as much as possible *)
