(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 & for_all2 pred tl1 tl2
  | (_, _) -> false

let rec filter pred =
  function
    [] ->
      []
  | a::l ->
      if pred a then
      	a::(filter pred l)
      else
      	filter pred l

let rec mem_assq x = function
    [] -> false
  | (a,b)::l -> a == x or mem_assq x l

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

(* File functions *)

let find_in_path path name =
  if Filename.is_absolute name then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* String functions *)

let capitalize s =
  let r = String.create (String.length s) in
  String.blit s 0 r 0 (String.length s);
  let c = s.[0] in
  if c >= 'a' & c <= 'z' then r.[0] <- Char.chr(Char.code c - 32);
  r

let lowercase s =
  let r = String.create (String.length s) in
  String.blit s 0 r 0 (String.length s);
  let c = s.[0] in
  if c >= 'A' & c <= 'Z' then r.[0] <- Char.chr(Char.code c + 32);
  r

(* File copy *)

let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = String.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

