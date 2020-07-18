(* TEST
   flags += " -nolabels "
*)

module A : module type of Array = ArrayLabels
module B : module type of Bytes = BytesLabels
module L : module type of List = ListLabels
module S : module type of String = StringLabels

module M : module type of struct include Map end [@remove_aliases] =
  MoreLabels.Map

module Se : module type of struct include Set end [@remove_aliases] =
  MoreLabels.Set

module H : module type of struct include Hashtbl end [@remove_aliases] =
  MoreLabels.Hashtbl

let ()  =
  ()
