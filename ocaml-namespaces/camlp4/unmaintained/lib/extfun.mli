(* camlp4r *)


(** Extensible functions.

   This module implements pattern matching extensible functions.
   To extend, use syntax [pa_extfun.cmo]:

      [extfun e with [ pattern_matching ]] *)

type t 'a 'b = 'x;
   (** The type of the extensible functions of type ['a -> 'b] *)
value empty : t 'a 'b;
   (** Empty extensible function *)
value apply : t 'a 'b -> 'a -> 'b;
   (** Apply an extensible function *)
exception Failure;
   (** Match failure while applying an extensible function *)
value print : t 'a 'b -> unit;
   (** Print patterns in the order they are recorded *)

(**/**)

type patt =
  [ Eapp of list patt
  | Eacc of list patt
  | Econ of string
  | Estr of string
  | Eint of string
  | Etup of list patt
  | Evar of unit ]
and expr 'a 'b = 'a -> option 'b
;

value extend : t 'a 'b -> list (patt * bool * expr 'a 'b) -> t 'a 'b;
