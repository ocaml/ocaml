(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The fields of type t are not mutable to preserve polymorphism of
   the empty stream. This is type safe because the empty stream is never
   patched. *)

type 'a t = {(*mutable*) count : int; (*mutable*) data : 'a data}
and 'a data =
    Sempty
  | Scons of 'a * 'a data
  | Sapp of 'a data * 'a data
  | Slazy of (unit -> 'a data)
  | Sgen of 'a gen
  | Sbuffio of buffio
and 'a gen =
  {mutable curr : 'a option option; func : int -> 'a option}
and buffio =
  {ic : in_channel; buff : string; mutable len : int; mutable ind : int}
exception Parse_failure
exception Parse_error of string

let count s = s.count

let fill_buff b =
  b.len <- input b.ic b.buff 0 (String.length b.buff);
  b.ind <- 0

let rec get_data =
  function
    Sempty -> None
  | Scons (a, d) -> Some (a, d)
  | Sapp (d1, d2) ->
      begin match get_data d1 with
        Some (a, d) -> Some (a, Sapp (d, d2))
      | None -> get_data d2
      end
  | Slazy f -> get_data (f ())
  | _ -> failwith "illegal stream concatenation"

let rec peek s =
  match s.data with
    Sempty -> None
  | Scons (a, _) -> Some a
  | Sapp (d1, d2) ->
      begin match get_data d1 with
        Some (a, d) ->
          Obj.set_field (Obj.repr s) 1 (Obj.repr (Scons (a, Sapp (d, d2))));
          Some a
      | None ->
          Obj.set_field (Obj.repr s) 1 (Obj.repr d2);
          peek s
      end
  | Slazy f ->
      begin match f () with
        Sgen _ | Sbuffio _ -> failwith "illegal stream concatenation"
      | x -> Obj.set_field (Obj.repr s) 1 (Obj.repr x); peek s
      end
  | Sgen {curr = Some a} -> a
  | Sgen g -> let x = g.func s.count in g.curr <- Some x; x
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then begin
        Obj.set_field (Obj.repr s) 1 (Obj.repr Sempty); None
      end
      else Some (Obj.magic b.buff.[b.ind])

let rec junk s =
  match s.data with
    Scons (_, s') ->
      Obj.set_field (Obj.repr s) 0 (Obj.repr (succ s.count));
      Obj.set_field (Obj.repr s) 1 (Obj.repr s')
  | Sgen {curr=Some None} -> ()
  | Sgen ({curr=Some _} as g) ->
      Obj.set_field (Obj.repr s) 0 (Obj.repr (succ s.count)); g.curr <- None
  | Sbuffio b ->
      Obj.set_field (Obj.repr s) 0 (Obj.repr (succ s.count));
      b.ind <- succ b.ind
  | _ -> match peek s with None -> () | Some _ -> junk s

let next s =
  match peek s with
    Some a -> junk s; a
  | None -> raise Parse_failure

let empty s =
  match peek s with
    Some _  -> raise Parse_failure
  | None -> ()

let iter f strm =
  let rec do_rec () =
    match peek strm with
      Some a -> junk strm; f a; do_rec ()
    | None -> ()
  in
  do_rec ()

(* Stream building functions *)

let from f = {count = 0; data = Sgen {curr = None; func = f}}

let of_list l =
  {count = 0; data = List.fold_right (fun x l -> Scons (x, l)) l Sempty}

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)

let of_channel ic =
  {count = 0;
   data = Sbuffio {ic = ic; buff = String.create 4096; len = 0; ind = 0}}

(* Stream expressions builders *)

let sempty = {count = 0; data = Sempty}
let scons f s = {count = 0; data = Slazy (fun _ -> Scons (f (), s.data))}
let sapp f s =
  match s.data with
    Sempty -> {count = 0; data = Slazy (fun _ -> (f ()).data)}
  | d -> {count = 0; data = Slazy (fun _-> Sapp ((f ()).data, d))}

(* For debugging use *)

let rec dump f s =
  print_string "{count = "; print_int s.count; print_string "; data = ";
  dump_data f s.data; print_string "}"; print_newline ()
and dump_data f =
  function
    Sempty -> print_string "Sempty"
  | Scons (a, d) ->
      print_string "Scons ("; f a; print_string ", "; dump_data f d;
      print_string ")"
  | Sapp (d1, d2) ->
      print_string "Sapp ("; dump_data f d1; print_string ", ";
      dump_data f d2; print_string ")"
  | Slazy f -> print_string "Slazy"
  | Sgen _ -> print_string "Sgen"
  | Sbuffio b -> print_string "Sbuffio"
