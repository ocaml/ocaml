(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Minimal support for Unicode characters in identifiers *)

type t = string

(* Non-ASCII letters that are allowed in identifiers (currently: Latin-9) *)

type case = Upper of int | Lower of int

let get_known_char c =
  match Uchar.to_int c with
  | 0xc0 (* À *) -> Some (Upper 0xe0 (* à *))
  | 0xe0 (* à *) -> Some (Lower 0xc0 (* À *))
  | 0xc1 (* Á *) -> Some (Upper 0xe1 (* á *))
  | 0xe1 (* á *) -> Some (Lower 0xc1 (* Á *))

  | 0xc2 (* Â *) -> Some (Upper 0xe2 (* â *))
  | 0xe2 (* â *) -> Some (Lower 0xc2 (* Â *))
  | 0xc3 (* Ã *) -> Some (Upper 0xe3 (* ã *))
  | 0xe3 (* ã *) -> Some (Lower 0xc3 (* Ã *))

  | 0xc4 (* Ä *) -> Some (Upper 0xe4 (* ä *))
  | 0xe4 (* ä *) -> Some (Lower 0xc4 (* Ä *))
  | 0xc5 (* Å *) -> Some (Upper 0xe5 (* å *))
  | 0xe5 (* å *) -> Some (Lower 0xc5 (* Å *))

  | 0xc6 (* Æ *) -> Some (Upper 0xe6 (* æ *))
  | 0xe6 (* æ *) -> Some (Lower 0xc6 (* Æ *))
  | 0xc7 (* Ç *) -> Some (Upper 0xe7 (* ç *))
  | 0xe7 (* ç *) -> Some (Lower 0xc7 (* Ç *))

  | 0xc8 (* È *) -> Some (Upper 0xe8 (* è *))
  | 0xe8 (* è *) -> Some (Lower 0xc8 (* È *))
  | 0xc9 (* É *) -> Some (Upper 0xe9 (* é *))
  | 0xe9 (* é *) -> Some (Lower 0xc9 (* É *))

  | 0xca (* Ê *) -> Some (Upper 0xea (* ê *))
  | 0xea (* ê *) -> Some (Lower 0xca (* Ê *))
  | 0xcb (* Ë *) -> Some (Upper 0xeb (* ë *))
  | 0xeb (* ë *) -> Some (Lower 0xcb (* Ë *))

  | 0xcc (* Ì *) -> Some (Upper 0xec (* ì *))
  | 0xec (* ì *) -> Some (Lower 0xcc (* Ì *))
  | 0xcd (* Í *) -> Some (Upper 0xed (* í *))
  | 0xed (* í *) -> Some (Lower 0xcd (* Í *))

  | 0xce (* Î *) -> Some (Upper 0xee (* î *))
  | 0xee (* î *) -> Some (Lower 0xce (* Î *))
  | 0xcf (* Ï *) -> Some (Upper 0xef (* ï *))
  | 0xef (* ï *) -> Some (Lower 0xcf (* Ï *))

  | 0xd0 (* Ð *) -> Some (Upper 0xf0 (* ð *))
  | 0xf0 (* ð *) -> Some (Lower 0xd0 (* Ð *))
  | 0xd1 (* Ñ *) -> Some (Upper 0xf1 (* ñ *))
  | 0xf1 (* ñ *) -> Some (Lower 0xd1 (* Ñ *))

  | 0xd2 (* Ò *) -> Some (Upper 0xf2 (* ò *))
  | 0xf2 (* ò *) -> Some (Lower 0xd2 (* Ò *))
  | 0xd3 (* Ó *) -> Some (Upper 0xf3 (* ó *))
  | 0xf3 (* ó *) -> Some (Lower 0xd3 (* Ó *))

  | 0xd4 (* Ô *) -> Some (Upper 0xf4 (* ô *))
  | 0xf4 (* ô *) -> Some (Lower 0xd4 (* Ô *))
  | 0xd5 (* Õ *) -> Some (Upper 0xf5 (* õ *))
  | 0xf5 (* õ *) -> Some (Lower 0xd5 (* Õ *))

  | 0xd6 (* Ö *) -> Some (Upper 0xf6 (* ö *))
  | 0xf6 (* ö *) -> Some (Lower 0xd6 (* Ö *))
  | 0xd8 (* Ø *) -> Some (Upper 0xf8 (* ø *))
  | 0xf8 (* ø *) -> Some (Lower 0xd8 (* Ø *))

  | 0xd9 (* Ù *) -> Some (Upper 0xf9 (* ù *))
  | 0xf9 (* ù *) -> Some (Lower 0xd9 (* Ù *))
  | 0xda (* Ú *) -> Some (Upper 0xfa (* ú *))
  | 0xfa (* ú *) -> Some (Lower 0xda (* Ú *))

  | 0xdb (* Û *) -> Some (Upper 0xfb (* û *))
  | 0xfb (* û *) -> Some (Lower 0xdb (* Û *))
  | 0xdc (* Ü *) -> Some (Upper 0xfc (* ü *))
  | 0xfc (* ü *) -> Some (Lower 0xdc (* Ü *))

  | 0xdd (* Ý *) -> Some (Upper 0xfd (* ý *))
  | 0xfd (* ý *) -> Some (Lower 0xdd (* Ý *))
  | 0xde (* Þ *) -> Some (Upper 0xfe (* þ *))
  | 0xfe (* þ *) -> Some (Lower 0xde (* Þ *))

  | 0x160 (* Š *) -> Some (Upper 0x161 (* š *))
  | 0x161 (* š *) -> Some (Lower 0x160 (* Š *))
  | 0x17d (* Ž *) -> Some (Upper 0x17e (* ž *))
  | 0x17e (* ž *) -> Some (Lower 0x17d (* Ž *))

  | 0x152 (* Œ *) -> Some (Upper 0x153 (* œ *))
  | 0x153 (* œ *) -> Some (Lower 0x152 (* Œ *))
  | 0x178 (* Ÿ *) -> Some (Upper 0xff (* ÿ *))
  | 0xff (* ÿ *) -> Some (Lower 0x178 (* Ÿ *))

  | 0x1e9e (* ẞ *) -> Some (Upper 0xdf (* ß *))
  | 0xdf (* ß *) -> Some (Lower 0x1e9e (* ẞ *))
  | _ -> None

(* NFD to NFC conversion table for the letters above *)

let get_known_pair c1 n2 =
  match Uchar.unsafe_to_char c1, Uchar.to_int n2 with
  | 'A', 0x300 -> Some 0xc0 (* À *)     | 'A', 0x301 -> Some 0xc1 (* Á *)
  | 'A', 0x302 -> Some 0xc2 (* Â *)     | 'A', 0x303 -> Some 0xc3 (* Ã *)
  | 'A', 0x308 -> Some 0xc4 (* Ä *)     | 'A', 0x30a -> Some 0xc5 (* Å *)
  | 'C', 0x327 -> Some 0xc7 (* Ç *)     | 'E', 0x300 -> Some 0xc8 (* È *)
  | 'E', 0x301 -> Some 0xc9 (* É *)     | 'E', 0x302 -> Some 0xca (* Ê *)
  | 'E', 0x308 -> Some 0xcb (* Ë *)     | 'I', 0x300 -> Some 0xcc (* Ì *)
  | 'I', 0x301 -> Some 0xcd (* Í *)     | 'I', 0x302 -> Some 0xce (* Î *)
  | 'I', 0x308 -> Some 0xcf (* Ï *)     | 'N', 0x303 -> Some 0xd1 (* Ñ *)
  | 'O', 0x300 -> Some 0xd2 (* Ò *)     | 'O', 0x301 -> Some 0xd3 (* Ó *)
  | 'O', 0x302 -> Some 0xd4 (* Ô *)     | 'O', 0x303 -> Some 0xd5 (* Õ *)
  | 'O', 0x308 -> Some 0xd6 (* Ö *)
  | 'U', 0x300 -> Some 0xd9 (* Ù *)     | 'U', 0x301 -> Some 0xda (* Ú *)
  | 'U', 0x302 -> Some 0xdb (* Û *)     | 'U', 0x308 -> Some 0xdc (* Ü *)
  | 'Y', 0x301 -> Some 0xdd (* Ý *)     | 'Y', 0x308 -> Some 0x178  (* Ÿ *)
  | 'S', 0x30c -> Some 0x160 (* Š *)    | 'Z', 0x30c -> Some 0x17d (* Ž *)
  | 'a', 0x300 -> Some 0xe0 (* à *)     | 'a', 0x301 -> Some 0xe1 (* á *)
  | 'a', 0x302 -> Some 0xe2 (* â *)     | 'a', 0x303 -> Some 0xe3 (* ã *)
  | 'a', 0x308 -> Some 0xe4 (* ä *)     | 'a', 0x30a -> Some 0xe5 (* å *)
  | 'c', 0x327 -> Some 0xe7 (* ç *)     | 'e', 0x300 -> Some 0xe8 (* è *)
  | 'e', 0x301 -> Some 0xe9 (* é *)     | 'e', 0x302 -> Some 0xea (* ê *)
  | 'e', 0x308 -> Some 0xeb (* ë *)     | 'i', 0x300 -> Some 0xec (* ì *)
  | 'i', 0x301 -> Some 0xed (* í *)     | 'i', 0x302 -> Some 0xee (* î *)
  | 'i', 0x308 -> Some 0xef (* ï *)     | 'n', 0x303 -> Some 0xf1 (* ñ *)
  | 'o', 0x300 -> Some 0xf2 (* ò *)     | 'o', 0x301 -> Some 0xf3 (* ó *)
  | 'o', 0x302 -> Some 0xf4 (* ô *)     | 'o', 0x303 -> Some 0xf5 (* õ *)
  | 'o', 0x308 -> Some 0xf6 (* ö *)
  | 'u', 0x300 -> Some 0xf9 (* ù *)     | 'u', 0x301 -> Some 0xfa (* ú *)
  | 'u', 0x302 -> Some 0xfb (* û *)     | 'u', 0x308 -> Some 0xfc (* ü *)
  | 'y', 0x301 -> Some 0xfd (* ý *)     | 'y', 0x308 -> Some 0xff (* ÿ *)
  | 's', 0x30c -> Some 0x161 (* š *)    | 'z', 0x30c -> Some 0x17e (* ž *)
  | _ -> None

let is_valid_decode_and_char decode uchar =
  Uchar.utf_decode_is_valid decode && uchar <> Uchar.rep

let rec pair_normalize ~buf ~valid s ~prev i =
  if i >= String.length s then begin
    Buffer.add_utf_8_uchar buf prev;
    valid
  end else begin
    let d = String.get_utf_8_uchar s i in
    let u = Uchar.utf_decode_uchar d in
    let valid = valid && is_valid_decode_and_char d u in
    let i' = i + Uchar.utf_decode_length d in
    match get_known_pair prev u with
    | Some u' ->
        let u' = Uchar.unsafe_of_int u' in
        pair_normalize ~buf ~valid s ~prev:u' i'
    | None ->
        Buffer.add_utf_8_uchar buf prev;
        pair_normalize ~buf ~valid s ~prev:u i'
  end

let normalize_map_first ?first s =
  (* [first : Uchar.t -> Uchar.t] is an optional function to be
       applied to the first character of [s] only.*)
  if String.is_empty s then Ok ""
  else
    let only_ascii = String.for_all Char.Ascii.is_valid s in
    (* get the first character of [s] *)
    let d = String.get_utf_8_uchar s 0 in
    let u0 = Uchar.utf_decode_uchar d in
    let i0 = Uchar.utf_decode_length d in
    let u0' = match first with None -> u0
                             | Some transform -> transform u0
    in
    if u0' = u0 && only_ascii then
      (* If the first character is unchanged by the [first] transformation,
         and the string is ascii-only, we can return it unchanged. *)
      Ok s
    else if only_ascii then begin
      (* If the first character is changed but the rest
         of the string is ascii-only, we can concatenate
         the new first character with the rest. *)
      let ulen = Uchar.utf_8_byte_length u0' in
      let restlen = String.length s - i0 in
      let res = Bytes.create (ulen + restlen) in
      let ulen' = Bytes.set_utf_8_uchar res 0 u0' in
      assert (ulen = ulen');
      BytesLabels.blit_string
        ~src:s ~src_pos:i0 ~dst:res ~dst_pos:ulen ~len:restlen;
      Ok (Bytes.unsafe_to_string res)
    end else begin
      (* Otherwise we are in the slow path where each character
         must be normalized. *)
      let buf = Buffer.create (String.length s) in
      let valid = is_valid_decode_and_char d u0 in
      let valid = pair_normalize ~buf ~valid s ~prev:u0' i0 in
      let contents = Buffer.contents buf in
      if valid then
        Ok contents
      else
        Error contents
    end

let normalize s =
  normalize_map_first s

(* Capitalization *)

let uchar_is_uppercase u =
  let c = Uchar.to_int u in
  if c < 0x80 then c >= 65 && c <= 90 else
    match get_known_char u with
    | Some(Upper _) -> true
    | _ -> false

let uchar_lowercase u =
  let c = Uchar.to_int u in
  if c < 0x80 then
    if c >= 65 && c <= 90 then Uchar.of_int (c + 32) else u
  else
    match get_known_char u with
    | Some(Upper u') -> Uchar.unsafe_of_int u'
    | _ -> u

let uchar_uppercase u =
  let c = Uchar.to_int u in
  if c < 0x80 then
    if c >= 97 && c <= 122 then Uchar.of_int (c - 32) else u
  else
    match get_known_char u with
    | Some(Lower u') -> Uchar.unsafe_of_int u'
    | _ -> u

let capitalize s =
  normalize_map_first ~first:uchar_uppercase s

let uncapitalize s =
  normalize_map_first ~first:uchar_lowercase s

let is_capitalized s =
  s <> "" &&
  uchar_is_uppercase (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))

(* Characters allowed in identifiers after normalization is applied.
   Currently:
     - ASCII letters, underscore
     - Latin-9 letters, represented in NFC
     - ASCII digits, single quote (but not as first character)
     - dot if [with_dot] = true
*)
let uchar_valid_in_identifier ~with_dot u =
  let c = Uchar.to_int u in
  if c < 0x80 then
       c >= 97 (* a *) && c <= 122 (* z *)
    || c >= 65 (* A *) && c <= 90 (* Z *)
    || c >= 48 (* 0 *) && c <= 57 (* 9 *)
    || c = 95 (* underscore *)
    || c = 39 (* single quote *)
    || (with_dot && c = 46) (* dot *)
  else
    match get_known_char u with
    | Some _ -> true
    | None -> false

let uchar_not_identifier_start u =
  let c = Uchar.to_int u in
     c >= 48 (* 0 *) && c <= 57 (* 9 *)
  || c = 39  (* single quote *)

(* Check whether a normalized string is a valid OCaml identifier. *)

type validation_result =
  | Valid
  | Invalid_character of Uchar.t   (** Character not allowed *)
  | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

let validate_identifier ?(with_dot=false) s =
  let rec check i =
    if i >= String.length s then Valid else begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let i' = i + Uchar.utf_decode_length d in
      if not (uchar_valid_in_identifier ~with_dot u) then
        Invalid_character u
      else if i = 0 && uchar_not_identifier_start u then
        Invalid_beginning u
      else
        check i'
    end
  in check 0

let is_valid_identifier s =
  validate_identifier s = Valid

let starts_like_a_valid_identifier s =
  s <> "" &&
  (let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0) in
   uchar_valid_in_identifier ~with_dot:false u
   && not (uchar_not_identifier_start u))

let is_lowercase s =
  let rec is_lowercase_at len s n =
    if n >= len then true
    else
      let d = String.get_utf_8_uchar s n in
      let u = Uchar.utf_decode_uchar d in
      (uchar_valid_in_identifier ~with_dot:false  u)
      && not (uchar_is_uppercase u)
      && is_lowercase_at len s (n+Uchar.utf_decode_length d)
  in
  is_lowercase_at (String.length s) s 0
