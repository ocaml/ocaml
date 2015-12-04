(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let fatal err =
  prerr_endline err;
  exit 2

module Make (S : sig
  module Key : sig
    type t
    val of_string : string -> t
    module Map : Map.S with type key = t
  end

  module Value : sig
    type t
    val of_string : string -> t
  end
end) = struct
  type parsed =
    | Always of S.Value.t
    | Variable of S.Value.t S.Key.Map.t

  let no_equals value =
    match String.index value '=' with
    | exception Not_found -> true
    | _index -> false

  let parse str ~help_text =
    match Misc.rev_split_words str ~separator:',' with
    | [] -> fatal help_text
    | [value] when no_equals value ->
      begin match S.Value.of_string value with
      | value -> Always value
      | exception exn ->
        fatal (Printf.sprintf "%s: %s" (Printexc.to_string exn) help_text)
      end
    | key_value_pairs ->
      let map =
        List.fold_left (fun map key_value_pair ->
            match String.index key_value_pair '=' with
            | exception Not_found -> fatal help_text
            | equals ->
              let length = String.length key_value_pair in
              if equals <= 0 || equals >= length - 1 then begin
                fatal help_text
              end;
              let key =
                let key = String.sub key_value_pair 0 equals in
                try S.Key.of_string key
                with exn ->
                  fatal (Printf.sprintf "%s: %s"
                    (Printexc.to_string exn) help_text)
              in
              let value =
                let value =
                  String.sub key_value_pair (equals + 1) (length - equals - 1)
                in
                try S.Value.of_string value
                with exn ->
                  fatal (Printf.sprintf "%s: %s"
                    (Printexc.to_string exn) help_text)
              in
              S.Key.Map.add key value map)
        S.Key.Map.empty
        key_value_pairs
      in
      Variable map

  let get ~key default parsed =
    match parsed with
    | Always fixed -> fixed
    | Variable by_key ->
        match S.Key.Map.find key by_key with
        | provided -> provided
        | exception Not_found ->
            default

end
