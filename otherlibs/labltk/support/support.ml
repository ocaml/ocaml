(* $Id$ *)

(* Parsing results of Tcl *)
(* List.split a string according to char_sep predicate *)
let split_str pred:char_sep str =
  let len = String.length str in
  let rec skip_sep cur =
    if cur >= len then cur
    else if char_sep str.[cur] then skip_sep (succ cur)
    else cur  in
  let rec split beg cur =
    if cur >= len then 
      if beg = cur then []
      else [String.sub str pos:beg len:(len - beg)]
    else if char_sep str.[cur] 
         then 
           let nextw = skip_sep cur in
            (String.sub str pos:beg len:(cur - beg))
              ::(split nextw nextw)
         else split beg (succ cur) in
  let wstart = skip_sep 0 in
  split wstart wstart

