(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, projet Cambium, College de France and Inria     *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Minimal support for Unicode characters in identifiers *)

(** Characters allowed in identifiers are, currently:
      - ASCII letters A-Z a-z
      - Latin-1 letters (U+00C0 - U+00FF except U+00D7 and U+00F7)
      - Character sequences which normalize to the above character under NFC
      - digits 0-9, underscore, single quote
*)

type t = string

val normalize: string -> (t,t) Result.t
(** Normalize the given UTF-8 encoded string.
    Invalid UTF-8 sequences results in a error and are replaced
    by U+FFFD.
    Identifier characters are put in NFC normalized form.
    Other Unicode characters are left unchanged. *)

val capitalize: string -> (t,t) Result.t
(** Like [normalize], but if the string starts with a lowercase identifier
    character, it is replaced by the corresponding uppercase character.
    Subsequent characters are not changed. *)

val uncapitalize: string -> (t,t) Result.t
(** Like [normalize], but if the string starts with an uppercase identifier
    character, it is replaced by the corresponding lowercase character.
    Subsequent characters are not changed. *)

val is_capitalized: t -> bool
(** Returns [true] if the given normalized string starts with an
    uppercase identifier character, [false] otherwise.  May return
    wrong results if the string is not normalized. *)

val is_valid_identifier: t -> bool
(** Check whether the given normalized string is a valid OCaml identifier:
    - all characters are identifier characters
    - it does not start with a digit or a single quote
*)

val is_lowercase: t -> bool
(** Returns [true] if the given normalized string only contains lowercase
    identifier character, [false] otherwise. May return wrong results if the
    string is not normalized. *)

type validation_result =
  | Valid
  | Invalid_character of Uchar.t   (** Character not allowed *)
  | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

val validate_identifier: ?with_dot:bool -> t -> validation_result
(** Like [is_valid_identifier], but returns a more detailed error code. Dots
    can be allowed to extend support to path-like identifiers. *)

val starts_like_a_valid_identifier: t -> bool
(** Checks whether the given normalized string starts with an identifier
    character other than a digit or a single quote.  Subsequent characters
    are not checked. *)
