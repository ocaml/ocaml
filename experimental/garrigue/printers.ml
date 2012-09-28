(* $Id: printers.ml 5492 2003-04-03 02:16:20Z garrigue $ *)

open Types

let ignore_abbrevs ppf ab =
  let s = match ab with
    Mnil -> "Mnil"
  | Mlink _ -> "Mlink _"
  | Mcons _ -> "Mcons _"
  in
  Format.pp_print_string ppf s
