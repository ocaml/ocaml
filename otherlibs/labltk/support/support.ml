(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels

(* Parsing results of Tcl *)
(* List.split a string according to char_sep predicate *)
let split_str ~pred:char_sep str =
  let len = String.length str in
  let rec skip_sep cur =
    if cur >= len then cur
    else if char_sep str.[cur] then skip_sep (succ cur)
    else cur  in
  let rec split beg cur =
    if cur >= len then 
      if beg = cur then []
      else [String.sub str ~pos:beg ~len:(len - beg)]
    else if char_sep str.[cur] 
         then 
           let nextw = skip_sep cur in
            (String.sub str ~pos:beg ~len:(cur - beg))
              ::(split nextw nextw)
         else split beg (succ cur) in
  let wstart = skip_sep 0 in
  split wstart wstart

(* Very easy hack for option type *)
let may f = function
  Some x -> Some (f x)
| None -> None

let maycons f x l =
  match x with
    Some x -> f x :: l
  | None -> l
