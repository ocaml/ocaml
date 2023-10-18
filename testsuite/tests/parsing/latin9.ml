(* TEST
 toplevel;
*)

(* NFC representation *)

let _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ
    = "ok"

type t = Æsop | Âcre | Ça | Élégant | Öst | Œuvre

let été = "summer"
let ça = "that"
let straße = "street"
let øre = "ear"

(* NFD representation *)

let f = function
  | Æsop -> 1 | Âcre -> 2 | Ça -> 3 | Élégant -> 4 | Öst -> 5 | Œuvre -> 6

let l = [été; ça; straße; øre]

let s = _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ

let () = assert (f Élégant (* NFC encoded *) = 4)

let () =
  let called = ref false in
  let élégant (* NFC encoded *) () = called := true in
  élégant (* NFD encoded *) (); assert (!called)
;;
(* The following two defs should error with 'Multiple definition…' *)
module Élégant (* NFC encoded *) = struct end
module Élégant (* NFD encoded *) = struct end;;

(** Quoted strings and extensions *)


let x = {où|x|où};;
let ko = {Là|x|Là};;

let x = {%âcre.name été|x|été};;
let x = {%Âcre.sub été|x|été};;

let x = {%âcre.m|x|};;

let%À.ça x = ();;

let x = (* {été|*)|été}*) ();;
let y = (* This is not a valid quoted string delimiter: {Été|*) ();;
