(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compare two bytecode executables for equality.
   Ignore loader prefix and debug infos. *)

open Printf

type cmpresult = Same | Differ of int

let rec cmpbytes ic1 ic2 len ofs =
  if len <= 0 then Same else begin
    let c1 = input_char ic1 and c2 = input_char ic2 in
    if c1 = c2 then cmpbytes ic1 ic2 (len - 1) (ofs + 1) else Differ ofs
  end

let skip_section (name : Bytesections.Name.t) =
  match name with
  | DBUG -> true
  | _ -> false

let cmpbyt file1 file2 =
  let open Bytesections in
  let ic1 = open_in_bin file1 in
  let toc1 = Bytesections.read_toc ic1 |> Bytesections.all in
  let ic2 = open_in_bin file2 in
  let toc2 = Bytesections.read_toc ic2 |> Bytesections.all in
  let rec cmpsections t1 t2 =
    match t1, t2 with
    | [], [] ->
        true
    | s1 :: t1, t2  when skip_section s1.name ->
        cmpsections t1 t2
    | t1, s2 :: t2  when skip_section s2.name ->
        cmpsections t1 t2
    | [], _  ->
        eprintf "%s has more sections than %s\n" file2 file1;
        false
    | _,  [] ->
        eprintf "%s has more sections than %s\n" file1 file2;
        false
    | s1 :: t1, s2 :: t2 ->
        let name1 = Bytesections.Name.to_string s1.name
        and name2 = Bytesections.Name.to_string s2.name in
        if name1 <> name2 then begin
          eprintf "Section mismatch: %s (in %s) / %s (in %s)\n"
                  name1 file1 name2 file2;
          false
        end else if s1.len <> s2.len then begin
          eprintf "Length of section %s differ: %d (in %s) / %d (in %s)\n"
                  name1 s1.len file1 s2.len file2;
          false
        end else begin
          seek_in ic1 s1.pos;
          seek_in ic2 s2.pos;
          match cmpbytes ic1 ic2 s1.len 0 with
          | Differ ofs ->
              eprintf "Files %s and %s differ: section %s, offset %d\n"
                      file1 file2 name1 ofs;
              false
          | Same ->
              cmpsections t1 t2
        end
  in
  let res = cmpsections toc1 toc2 in
  close_in ic1; close_in ic2;
  res

let main () =
  if Array.length Sys.argv <> 3 then begin
    eprintf "Usage: cmpbyt <file 1> <file 2>\n";
    exit 2
  end;
  if cmpbyt Sys.argv.(1) Sys.argv.(2) then exit 0 else exit 1

let _ = main ()
