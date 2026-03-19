(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Abstract Syntax Tree for the Tests Specification Language *)

type 'a located = {
  node : 'a;
  loc : Location.t
}

type environment_statement = (* <env update> ::= *)
  | Assignment of bool * string located * string located
    (* <variable> = <value> *)
  | Append of string located * string located
    (* <variable> += <value> *)
  | Include of string located
    (* include <named environment> *)
  | Unset of string located
    (* clear <variable> *)

type action = {
  name: string located;
  modifiers: string located list;
} (* <action> ::= <name> [with <named environment>, <named environment>....] *)

type statement =
  | Environment_statement of environment_statement located (* <env update> *)
  | Action of action (* <action> *)
  | Not of statement (* not <statement> *)
  | And of statement * statement (* <statement> && <statement> *)
  | Or of statement * statement (* <statement> || <statement> *)
  | If of statement * statement * statement option
    (* if <statement> then <statement> [else <statement>] *)

type t = Ast of statement list * t list
(* <block> ::=
     <statement>; <statement>; ...; { <block> } { <block> } ... *)

val split_env :
  statement list -> environment_statement located list * statement list

val make_identifier : ?loc:Location.t -> string -> string located
val make_string : ?loc:Location.t -> string -> string located
val make_environment_statement :
  ?loc:Location.t -> environment_statement -> environment_statement located
