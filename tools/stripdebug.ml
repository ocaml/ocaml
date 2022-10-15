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

(* Copy a bytecode executable, removing debugging information
   and #! header from the copy.
   Usage: stripdebug <source file> <dest file>
*)

open Printf
open Misc

let stripdebug infile outfile =
  let ic = open_in_bin infile in
  let toc = Bytesections.read_toc ic in
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
                 outfile in
  (* Skip the #! header, going straight to the first section. *)
  (* Copy each section except DBUG and CRCS *)
  let toc_writer = Bytesections.init_record oc in
  List.iter
    (fun {Bytesections.name; pos; len} ->
       match (name :> string) with
       | "DBUG" | "CRCS" -> ()
       | _ ->
           seek_in ic pos;
           copy_file_chunk ic oc len;
           Bytesections.record toc_writer name
    )
    toc;
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer toc_writer;
  (* Done *)
  close_in ic;
  close_out oc

let _ =
  if Array.length Sys.argv = 3
  then stripdebug Sys.argv.(1) Sys.argv.(2)
  else begin
    eprintf "Usage: stripdebug <source file> <destination file>\n";
    exit 2
  end
