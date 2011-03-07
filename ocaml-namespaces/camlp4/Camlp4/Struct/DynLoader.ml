(* camlp4r pa_macro.cmo *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2001-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)




type t = Queue.t string;

exception Error of string and string;

value include_dir x y = Queue.add y x;

value fold_load_path x f acc = Queue.fold (fun x y -> f y x) acc x;

value mk ?(ocaml_stdlib = True) ?(camlp4_stdlib = True) () =
  let q = Queue.create () in do {
    if ocaml_stdlib then include_dir q Camlp4_config.ocaml_standard_library else ();
    if camlp4_stdlib then do {
      include_dir q Camlp4_config.camlp4_standard_library;
      include_dir q (Filename.concat Camlp4_config.camlp4_standard_library "Camlp4Parsers");
      include_dir q (Filename.concat Camlp4_config.camlp4_standard_library "Camlp4Printers");
      include_dir q (Filename.concat Camlp4_config.camlp4_standard_library "Camlp4Filters");
    } else ();
    include_dir q ".";
  q
};

(* Load files in core *)

value find_in_path x name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let res =
      fold_load_path x
        (fun dir ->
          fun
          [ None ->
              let fullname = Filename.concat dir name in
              if Sys.file_exists fullname then Some fullname else None
          | x -> x ]) None
    in match res with [ None -> raise Not_found | Some x -> x ];

value load =
  let _initialized = ref False in
  fun _path file ->
    do {
      if not _initialized.val then
        try do {
          Dynlink.init ();
          Dynlink.allow_unsafe_modules True;
         _initialized.val := True
        }
        with
        [ Dynlink.Error e ->
           raise (Error "Camlp4's dynamic loader initialization" (Dynlink.error_message e)) ]
      else ();
      let fname =
        try find_in_path _path file with
        [ Not_found -> raise (Error file "file not found in path") ]
      in
      try Dynlink.loadfile fname with
      [ Dynlink.Error e -> raise (Error fname (Dynlink.error_message e)) ]
    };


value is_native = Dynlink.is_native;
