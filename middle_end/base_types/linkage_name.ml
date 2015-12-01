(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module T = struct
  include String
  let hash = Hashtbl.hash
  let print ppf t = Format.fprintf ppf "%s" t
  let output chan t = output_string chan t
end

include T
include Ext_types.Identifiable.Make (T)

let create t = t
let to_string t = t

let to_shortened_string t =
  if not Sys.win32 then t
  else begin
    (* The Microsoft assembler has a 247-character limit on symbol names. *)
    let max_length = 247 in
    if String.length t <= max_length then t
    else begin
      let suffix = "_long_name" in
      let suffix_length = String.length suffix in
      let prefix_length = max_length - suffix_length in
      let prefix = String.sub t 0 prefix_length in
      prefix ^ suffix
    end
  end
