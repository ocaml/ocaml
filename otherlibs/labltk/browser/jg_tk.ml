(* $Id$ *)

open Tk

let tpos x : textIndex = `Linechar (1,0), [`Char x]
and tposend x : textIndex = `End, [`Char (-x)]
let tstart : textIndex = `Linechar (1,0), []
and tend : textIndex = `End, []
