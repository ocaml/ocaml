(***********************************************************************)
(*                                                                     *)
(*                             Ocaml                                   *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The fields of type t are not mutable to preserve polymorphism of
   the empty stream. This is type safe because the empty stream is never
   patched. *)

type 'a t = { count : int; data : 'a data }
and 'a data =
    Sempty
  | Scons of 'a * 'a data
  | Sapp of 'a data * 'a data
  | Slazy of (unit -> 'a data)
  | Sgen of 'a gen
  | Sbuffio of buffio
and 'a gen = { mutable curr : 'a option option; func : int -> 'a option }
and buffio =
  { ic : in_channel; buff : string; mutable len : int; mutable ind : int }
;;
exception Failure;;
exception Error of string;;

external count : 'a t -> int = "%field0";;
external set_count : 'a t -> int -> unit = "%setfield0";;
let set_data (s : 'a t) (d : 'a data) =
  Obj.set_field (Obj.repr s) 1 (Obj.repr d)
;;

let fill_buff b =
  b.len <- input b.ic b.buff 0 (String.length b.buff); b.ind <- 0
;;

let rec get_data =
  function
    Sempty -> None
  | Scons (a, d) -> Some (a, d)
  | Sapp (d1, d2) ->
      begin match get_data d1 with
        Some (a, d1) -> Some (a, Sapp (d1, d2))
      | None -> get_data d2
      end
  | Slazy f ->
      begin match f () with
        Sgen _ | Sbuffio _ -> failwith "illegal stream concatenation"
      | x -> get_data x
      end
  | Sgen _ | Sbuffio _ ->
      failwith "illegal stream concatenation"
;;

let rec peek s =
  match s.data with
    Sempty -> None
  | Scons (a, _) -> Some a
  | Sapp (_, _) ->
      begin match get_data s.data with
        Some (a, d) -> set_data s (Scons (a, d)); Some a
      | None -> None
      end
  | Slazy f ->
      begin match f () with
        Sgen _ | Sbuffio _ -> failwith "illegal stream concatenation"
      | d -> set_data s d; peek s
      end
  | Sgen {curr = Some a} -> a
  | Sgen g -> let x = g.func s.count in g.curr <- Some x; x
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then begin set_data s Sempty; None end
      else Some (Obj.magic (String.unsafe_get b.buff b.ind))
;;

let rec junk s =
  match s.data with
    Scons (_, d) -> set_count s (succ s.count); set_data s d
  | Sgen ({curr = Some _} as g) -> set_count s (succ s.count); g.curr <- None
  | Sbuffio b -> set_count s (succ s.count); b.ind <- succ b.ind
  | _ ->
      match peek s with
        None -> ()
      | Some _ -> junk s
;;

let rec nget n s =
  if n <= 0 then [], s.data, 0
  else
    match peek s with
      Some a ->
        junk s;
        let (al, d, k) = nget (pred n) s in a :: al, Scons (a, d), succ k
    | None -> [], s.data, 0
;;

let npeek n s =
  let (al, d, len) = nget n s in set_count s (s.count - len); set_data s d; al
;;

let next s =
  match peek s with
    Some a -> junk s; a
  | None -> raise Failure
;;

let empty s =
  match peek s with
    Some _ -> raise Failure
  | None -> ()
;;

let iter f strm =
  let rec do_rec () =
    match peek strm with
      Some a -> junk strm; ignore(f a); do_rec ()
    | None -> ()
  in
  do_rec ()
;;

(* Stream building functions *)

let from f = {count = 0; data = Sgen {curr = None; func = f}};;

let of_list l =
  {count = 0; data = List.fold_right (fun x l -> Scons (x, l)) l Sempty}
;;

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)
;;

let of_channel ic =
  {count = 0;
   data = Sbuffio {ic = ic; buff = String.create 4096; len = 0; ind = 0}}
;;

(* Stream expressions builders *)

let iapp i s = {count = 0; data = Sapp (i.data, s.data)};;
let icons i s = {count = 0; data = Scons (i, s.data)};;
let ising i = {count = 0; data = Scons (i, Sempty)};;

let lapp f s =
  {count = 0; data = Slazy (fun _ -> Sapp ((f ()).data, s.data))}
;;
let lcons f s = {count = 0; data = Slazy (fun _ -> Scons (f (), s.data))};;
let lsing f = {count = 0; data = Slazy (fun _ -> Scons (f (), Sempty))};;

let sempty = {count = 0; data = Sempty};;
let slazy f = {count = 0; data = Slazy (fun _ -> (f ()).data)};;

(* For debugging use *)

let rec dump f s =
  print_string "{count = ";
  print_int s.count;
  print_string "; data = ";
  dump_data f s.data;
  print_string "}";
  print_newline ()
and dump_data f =
  function
    Sempty -> print_string "Sempty"
  | Scons (a, d) ->
      print_string "Scons (";
      f a;
      print_string ", ";
      dump_data f d;
      print_string ")"
  | Sapp (d1, d2) ->
      print_string "Sapp (";
      dump_data f d1;
      print_string ", ";
      dump_data f d2;
      print_string ")"
  | Slazy f -> print_string "Slazy"
  | Sgen _ -> print_string "Sgen"
  | Sbuffio b -> print_string "Sbuffio"
;;
