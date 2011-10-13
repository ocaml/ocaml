(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

let id = << fun x -> x >>
(* Imported and traduced from CCT *)
let zero = << fun s -> fun z -> z >>
let succ = << fun n -> fun s -> fun z -> s n >>
let one = << $succ$ $zero$ >>
let iota = << fun x -> z >>
let rho = << fun m -> fun r -> (s m (m r $iota$ r)) >>
let rec_nat =
  << fun n -> fun s -> fun z -> n $rho$ $iota$ $rho$ >>
let plus = << fun n -> fun m -> $rec_nat$ n (fun n -> fun p -> $succ$ p) m >>
let times = << fun n -> fun m -> $rec_nat$ n (fun n -> fun p -> $plus$ m p) $zero$ >>
let fact = << fun n -> $rec_nat$ n (fun n -> fun p -> $times$ ($succ$ n) p) $one$ >>
