(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

let lt_string ?(nocase=false) s1 s2 =
  if nocase then String.lowercase s1 < String.lowercase s2 else s1 < s2

class completion ?nocase texts = object
  val mutable texts = texts
  val nocase = nocase
  val mutable prefix = ""
  val mutable current = 0
  method add c =
    prefix <- prefix ^ c;
    while current < List.length texts - 1 &&
      lt_string (List.nth texts current) prefix ?nocase
    do
      current <- current + 1
    done;
    current
  method current = current
  method get_current = List.nth texts current
  method reset =
    prefix <- "";
    current <- 0
end

class timed ?nocase ?wait texts = object (self)
  inherit completion texts ?nocase as super
  val wait = match wait with None -> 500 | Some n -> n
  val mutable timer = None
  method add c =
    begin match timer with
      None -> self#reset
    | Some t -> Timer.remove t
    end;
    timer <- Some (Timer.add ~ms:wait ~callback:(fun () -> self#reset));
    super#add c
  method reset =
    timer <- None; super#reset
end
