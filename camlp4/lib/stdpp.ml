(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Exc_located of (int * int) and exn;

value raise_with_loc loc exc =
  match exc with
  [ Exc_located _ _ -> raise exc
  | _ -> raise (Exc_located loc exc) ]
;

value line_of_loc fname (bp, ep) =
  try
    let ic = open_in_bin fname in
    let rec loop lin col cnt =
      if cnt < bp then
        let (lin, col) =
          match input_char ic with
          [ '\n' -> (lin + 1, 0)
          | _ -> (lin, col + 1) ]
        in
        loop lin col (cnt + 1)
      else (lin, col, col + ep - bp)
    in
    let r = try loop 1 0 0 with [ End_of_file -> (1, bp, ep) ] in
    do { close_in ic; r }
  with
  [ Sys_error _ -> (1, bp, ep) ]
;

value loc_name = ref "loc";
