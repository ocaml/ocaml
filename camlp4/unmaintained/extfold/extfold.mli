(* camlp4r *)
(* $Id$ *)

type t 'te 'a 'b =
  Gramext.g_entry 'te -> list (Gramext.g_symbol 'te) ->
    (Stream.t 'te -> 'a) -> Stream.t 'te -> 'b
;

type tsep 'te 'a 'b =
  Gramext.g_entry 'te -> list (Gramext.g_symbol 'te) ->
    (Stream.t 'te -> 'a) -> (Stream.t 'te -> unit) -> Stream.t 'te -> 'b
;

value sfold0 : ('a -> 'b -> 'b) -> 'b -> t _ 'a 'b;
value sfold1 : ('a -> 'b -> 'b) -> 'b -> t _ 'a 'b;
value sfold0sep : ('a -> 'b -> 'b) -> 'b -> tsep _ 'a 'b;
value sfold1sep : ('a -> 'b -> 'b) -> 'b -> tsep _ 'a 'b;

value slist0 : t _ 'a (list 'a);
value slist1 : t _ 'a (list 'a);
value slist0sep : tsep _ 'a (list 'a);
value slist1sep : tsep _ 'a (list 'a);

value sopt : t _ 'a (option 'a);
