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

let remove_section (s : Bytesections.Name.t) =
  match s with
  | DBUG -> !remove_DBUG
  | CRCS -> !remove_CRCS
  | _ -> false

let stripdebug infile outfile =
  let ic = open_in_bin infile in
  let toc = Bytesections.read_toc ic in
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
                 outfile in
  if not !remove_header then begin
    (* Copy header up to first section *)
    seek_in ic 0;
    let header_length = Bytesections.pos_first_section toc in
    copy_file_chunk ic oc header_length
  end;
  (* Copy each section except DBUG and CRCS *)
  let toc_writer = Bytesections.init_record oc in
  List.iter
    (fun {Bytesections.name; pos; len} ->
       if not (remove_section name) then begin
         seek_in ic pos;
         copy_file_chunk ic oc len;
         Bytesections.record toc_writer name
       end
    )
    (Bytesections.all toc);
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer toc_writer;
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
