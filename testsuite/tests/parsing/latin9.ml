(* TEST *)

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
