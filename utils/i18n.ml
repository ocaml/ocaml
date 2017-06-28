(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, Inria Paris                             *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Internationalization and localization plugin hooks *)

(** translated string type *)
type s = string

(** I18n implementation *)

type implementation =
  {

    kdprintf:
      'a 'r.
         ( (Format.formatter -> unit) -> 'r)
      -> ('a, Format.formatter,unit, 'r) format4 -> 'a;

    kfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> ('a,Format.formatter,unit,'r) format4-> 'a;
      (** Singular form printing function *)

    kdnprintf:
      'a 'r.
         ((Format.formatter -> unit) -> 'r)
      -> int -> (('a, Format.formatter,unit, 'r) format4 as 'f) -> 'f -> 'a;

    knfprintf:
      'a 'r. (Format.formatter -> 'r) -> Format.formatter
      -> int -> (('a,Format.formatter,unit,'r) format4 as 'f)
      -> 'f -> 'a;
    (** Plural form printing function *)

    text: Format.formatter -> string -> unit

  }

let choice n x y =
  (* folloxing gettext convention, the default behavior is assumed to be the
     germanic language with a singular(=1) and non-singular form
     (i.e. {0} union [2,+infinity]) *)
  if n = 1 then x else y

(** The default implementation only convert type *)
let default = {
  kdprintf = Format.kdprintf;
  kdnprintf = (fun k n singular plural ->
      Format.kdprintf k (choice n singular plural)
    );
  kfprintf = Format.kfprintf;
  knfprintf = (fun k ppf n x y -> Format.kfprintf k ppf (choice n x y) );
  text = Format.pp_print_text
}

let hook = ref default
(** Plugin hook for i18n implementation *)


let kfprintf k ppf fmt = !hook.kfprintf k ppf fmt

let ksprintf k fmt =
  let b = Buffer.create 10 in
  let ppf = Format.formatter_of_buffer b in
  kfprintf (fun ppf -> Format.pp_print_flush ppf (); k (Buffer.contents b))
    ppf fmt
let sprintf fmt = ksprintf (fun x -> x) fmt
let formattify x =
  let open CamlinternalFormatBasics in
  Format(String_literal (x,End_of_format), x)

let s x = sprintf (formattify x)

let i18n x = s x

let fprintf ppf fmt = !hook.kfprintf ignore ppf fmt
let printf fmt = fprintf  (Format.std_formatter) fmt
let eprintf fmt = fprintf (Format.std_formatter) fmt
let kdprintf k fmt = !hook.kdprintf k fmt
let dprintf fmt = kdprintf (fun x -> x) fmt
let kfnprintf k ppf n fmt fmt2 =
  !hook.knfprintf k ppf n fmt fmt2
let dnprintf n singular plural = !hook.kdnprintf (fun x -> x) n singular plural
let fnprintf x = kfnprintf ignore x
let snprintf n fmt fmt' =
  let b = Buffer.create 10 in
  let ppf = Format.formatter_of_buffer b in
  kfnprintf (fun ppf -> Format.pp_print_flush ppf (); Buffer.contents b)
    ppf n fmt fmt'

let sn n x y = snprintf n (formattify x) (formattify y)


let raw x = x
let to_string x = x

let pp ppf s = Format.pp_print_string ppf (i18n s)
let pp_print_text ppf s = !hook.text ppf s
