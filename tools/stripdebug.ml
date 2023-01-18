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

(* Copy a bytecode executable, removing debugging information and possibly
   dynlink information and #! header from the copy.
*)

open Misc

let remove_header = ref false
let remove_DBUG = ref true
let remove_CRCS = ref false

let remove_section = function
  | "DBUG" -> !remove_DBUG
  | "CRCS" -> !remove_CRCS
  | _ -> false

let stripdebug infile outfile =
  let ic = open_in_bin infile in
  Bytesections.read_toc ic;
  let toc = Bytesections.toc() in
  let pos_first_section = Bytesections.pos_first_section ic in
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
                 outfile in
  if !remove_header then begin
    (* Skip the #! header, going straight to the first section. *)
    seek_in ic pos_first_section
  end else begin
    (* Copy header up to first section *)
    seek_in ic 0;
    copy_file_chunk ic oc pos_first_section
  end;
  (* Copy each section except those to be removed *)
  Bytesections.init_record oc;
  List.iter
    (fun (name, len) ->
      if remove_section name then begin
        seek_in ic (pos_in ic + len)
      end else begin
        copy_file_chunk ic oc len;
        Bytesections.record oc name
      end)
    toc;
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer oc;
  (* Done *)
  close_in ic;
  close_out oc

let options = [
  "-remove-header", Arg.Set remove_header,
     "remove the header that calls ocamlrun automatically";
  "-keep-header", Arg.Clear remove_header,
     "preserve the header that calls ocamlrun automatically (default)";
  "-remove-debug", Arg.Set remove_DBUG,
     "remove all debugging information (default)";
  "-keep-debug", Arg.Clear remove_DBUG,
     "preserve all debugging information";
  "-remove-dynlink", Arg.Set remove_CRCS,
     "remove the data needed for dynamic code loading";
  "-keep-dynlink", Arg.Clear remove_CRCS,
     "preserve the data needed for dynamic code loading (default)";
  "-all", Arg.Unit (fun () -> remove_header := true; remove_DBUG := true;
                              remove_CRCS := true),
     "remove header, debugging info, and dynamic code loading info"
]

let usage =
"Usage: stripdebug [options] <input file> <output file>\n\
Options are:"

let main() =
  let anon = ref [] in
  Arg.parse options (fun x -> anon := x :: !anon) usage;
  match !anon with
  | [output; input] -> stripdebug input output
  | _ -> Arg.usage options usage; exit 2

let _ = main()
