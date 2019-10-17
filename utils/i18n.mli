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


(** Localization hook *)

(**

   This module provides a compiler-libs hook for localizing
   OCaml user-facing messages.

   More precisely, this module contains localizable version of the
   printing functions of Format( [fprintf], [dprintf], [kprintf], ...),
   together with an overridable implementation {!hook} for those functions.

   Those functions are expected to be used whenever we print a message intended
   to an user. For instance

   {[
     I18n.fprintf ppf
       "Variable %s must occur on both sides of this | pattern"
       (Ident.name id)
   ]}

   The default hook merely calls back the corresponding {!Format} functions.
   Localization-aware compiler-libs clients can override this implementation
   to perform translation on the fly of the format string.
   for instance, one possible implementation is to replace
   [ "Variable %s must occur on both sides of this | pattern"]
   with [" "A variavel %s deve ocorrer em ambos os lados deste | pattern"].

   This module also provides anabstract type {!s} to represent a stored
   translated text for compatibility reason.
   This abstract type can only be printed, because no
   string operations works on all human languages simultaneously.

   On a first approximation, using this module is only a matter of using
   [I18n.fprintf] whenever we are printing a grammatically correct text,
   and [Format.fprintf] otherwise. There is however an important rule to provide
   nicely translatable message for translators:
   the smallest translatable unit is the phrase.
   Any smaller grammatical component (nouns, adjectives, verbs, ...)
   may create unstranlatable messages due to incompatible contexts.

   A classic example of a problematic format string is
   {["the %a %s is defined in both types %s and %s.]}
   where the first [%a] is supposed to be a translation for "label" or
   "constructor". However, it is not always possible to use this
   translations to construct a grammatically correct sentence.
   As an illustration the French translations for those two cases
   {["l'etiquette %s est definie dans les deux types %s and %s"]}
   or
   {["Le constructeur %s est defini dans les deux types %s and %s"]}
   Notice that the declensions of the translation of "the", "l'" and "le", are
   not the same in the two cases, and the French translator ends up stuck.

   To avoid this issue is often better to not generate partial text fragment
   and use variants to carry the information to the error or warning reporter.
   This reporter should have enough information to construct well-formed and
   translatable sentences.

   In particular, the stored translated type {!s} is currently
   misused at few places, and it may be better to remove it completely
   in the future.

   Note that this module should not be opened:
   most translation framework requires the ability to syntactically
   distinguish user-facing string constants that should be translated
   when scanning the code base. To make this scan relatively easy, we
   are currently using localization-aware function in a fully-qualified.

   Note for extractions tools:
   Extractions tools should be able to extract translatable message from
   the arguments of all function defined in this module and
   {!Location.raise_errorf}, {!Location.errorf} and {!Location.msg}.

*)


(** {1 Convenience functions}
    These functions share the same interface as {!Format}'s function but use the
    implementation provided by {!hook}
*)

val kfprintf: (Format.formatter -> 'r) -> Format.formatter
  -> ('a,Format.formatter,unit,'r) format4  -> 'a
val fprintf: Format.formatter -> ('a,Format.formatter,unit) format  -> 'a
val eprintf: ('a,Format.formatter,unit) format  -> 'a
val printf: ('a,Format.formatter,unit) format  -> 'a
val kdprintf:
  ((Format.formatter-> unit) -> 'r)
  -> ('a,Format.formatter, unit, 'r) format4 -> 'a
val dprintf: ('a,Format.formatter, unit, Format.formatter -> unit) format4 -> 'a


(** {2 Plural variants}

    Translating messages with varying grammatical number is hard.

    Indeed, not all languages only have a singular and a plural. For
    instance, few indo-european language have preserved the dual, a special
    case for two objects.
    If one consider ordinals (1st, 2nd, 3rd, 4th, 11th, 21st, 22nd, ...) this
    problem appears even in English..
    In order to try to make it possible to translate such sentences, we provide
    a plural version for each Format's printing functions that
    provides more information to the translator:
    - the numerical number of varying entities
    - a default text for the singular case
    - a default text for the plural case.

*)


val fnprintf:
  Format.formatter
  -> int
  -> ( ('a,Format.formatter,unit) format as 'f)
  -> 'f
  -> 'a
(** [fnprintf num ppf singular plural] *)

val dnprintf:
  int
  -> ( ('a,Format.formatter, unit, Format.formatter -> unit) format4 as 'f)
  -> 'f
  -> 'a
(** [dnprintf num singular plural] *)






(** {1 Translated string}
  A type for already translated string.
  This type is used for compatibility reason when functions expect
  fully rendered strings rather than printer
*)
type s


(** Those three functions can be used to translate and store
    an user-facing message *)
val s: string -> s
val i18n: string -> s
val sn: int -> string -> string -> s
  (** [sn num singular plural] *)

(** Store the result of a format string into a {!s} value *)
val ksprintf: (s -> 'r) -> ('a,Format.formatter,unit,'r) format4 -> 'a
val sprintf: ('a,Format.formatter,unit,s) format4 -> 'a
val snprintf:
  int -> ( ('a,Format.formatter,unit,s) format4 as 'f) -> 'f -> 'a
(** [snprintf num singular plural] *)

val pp: Format.formatter -> s -> unit
(** The printing function for stored translation *)


val pp_print_text: Format.formatter -> s -> unit
(** [pp_print_text] is here because the notion of space differs from language,
     it is thus better to let the translation layer handle the layouting here *)


(** {1 Conversion function}
    Convert back-and-forth to standard string *)
val to_string: s -> string

(**  [raw] breaks localization, uses as little as possible *)
val raw: string -> s


(** {1 Localizatio hook} *)

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
    (** Pretty printed text (see {!Format.pp_print_text}) *)

  }

(** The default implementation calls the original [Format] functions directly *)
val default:implementation

(** Hook for localization implementation *)
val hook : implementation ref
