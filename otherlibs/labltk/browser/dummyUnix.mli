(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

module Mutex : sig
  type t
  external create : unit -> t = "%ignore"
  external lock : t -> unit = "%ignore"
  external unlock : t -> unit = "%ignore"
end

module Thread : sig
  type t
  external create : ('a -> 'b) -> 'a -> t = "caml_input"
end

module ThreadUnix : sig
  open Unix
  external read : file_descr -> buf:string -> pos:int -> len:int -> int
      = "caml_input"
end
