(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

(** Base class for map traversal, it includes some builtin types. *)
class c = object
  method string x : string = x;
  method int x : int = x;
  method float x : float = x;
  method bool x : bool = x;
  method list : ! 'a 'b . ('a -> 'b) -> list 'a -> list 'b =
    List.map;
  method option : ! 'a 'b . ('a -> 'b) -> option 'a -> option 'b =
    fun f -> fun [ None -> None | Some x -> Some (f x) ];
  method array : ! 'a 'b . ('a -> 'b) -> array 'a -> array 'b =
    Array.map;
  method ref : ! 'a 'b . ('a -> 'b) -> ref 'a -> ref 'b =
    fun f { val = x } -> { val = f x };
end;
