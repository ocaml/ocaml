(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type 'a t = {mutable count : int; mutable data : 'a data}
and 'a data =
    Sempty
  | Scons of 'a * 'a data
  | Sapp of 'a data * 'a data
  | Sfunc of (int -> 'a data)
  | Sbuffio of buffio
and buffio =
  {ic : in_channel; buff : string; mutable len : int; mutable ind : int}
exception Parse_failure
exception Parse_error of string

let count s = s.count

let fill_buff b =
  b.len <- input b.ic b.buff 0 (String.length b.buff);
  b.ind <- 0

let rec get_data cnt =
  function
    Sempty -> None
  | Scons (a, d) -> Some (a, d)
  | Sapp (d1, d2) ->
      begin match get_data cnt d1 with
        Some (a, d) -> Some (a, Sapp (d, d2))
      | None -> get_data cnt d2
      end
  | Sfunc f -> get_data cnt (f cnt)
  | Sbuffio b as s ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then None else Some (Obj.magic b.buff.[b.ind], s)

let rec peek s =
  match s.data with
    Sempty -> None
  | Scons (a, _) -> Some a
  | Sapp (d1, d2) ->
      begin match get_data s.count d1 with
        Some (a, d) -> s.data <- Scons (a, Sapp (d, d2)); Some a
      | None -> s.data <- d2; peek s
      end
  | Sfunc f ->
      s.data <- f s.count;
      peek s
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then begin s.data <- Sempty; None end
      else Some (Obj.magic b.buff.[b.ind])

let rec junk s =
  match s.data with
    Scons (_, s') -> s.count <- succ s.count; s.data <- s'
  | Sbuffio b -> s.count <- succ s.count; b.ind <- succ b.ind
  | _ -> ()

let next s =
  match peek s with
    Some a -> junk s; a
  | None -> raise Parse_failure

let empty s =
  match peek s with
    Some _  -> raise Parse_failure
  | None -> ()


let from f =
  let rec g c =
    match f c with
      Some a -> Scons (a, Sfunc g)
    | None -> Sempty
  in
  {count = 0; data = Sfunc g}

let of_list l =
  {count = 0; data = List.fold_right (fun x l -> Scons (x, l)) l Sempty}

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)

let of_channel ic =
  {count = 0;
   data = Sbuffio {ic = ic; buff = String.create 4096; len = 0; ind = 0}}

let sempty = {count = 0; data = Sempty}
let scons f s = {count = 0; data = Sfunc (fun _ -> Scons (f (), s.data))}
let sapp f s =
  match s.data with
    Sempty -> {count = 0; data = Sfunc (fun _ -> (f ()).data)}
  | d -> {count = 0; data = Sfunc (fun _-> Sapp ((f ()).data, d))}

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
  | Sfunc f -> print_string "Sfunc"
  | Sbuffio b -> print_string "Sbuffio"
