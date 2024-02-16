(* TEST *)

open Printf
open Effect
open Effect.Deep

(** {1 Resumable exceptions} *)

type _ eff += Conversion_failure : string -> int eff

let int_of_string s =
  match int_of_string_opt s with
  | Some n -> n
  | None -> perform (Conversion_failure s)

let sum_stringlist l =
  l |> List.map int_of_string |> List.fold_left (+) 0

let safe_sum_stringlist l =
  match sum_stringlist l with
  | v -> v
  | effect Conversion_failure(s), k ->
      printf "Bad input %s, replaced with 0\n" s;
      continue k 0

let _ =
  printf "Sum is: %d\n" (safe_sum_stringlist ["1"; "xxx"; "2"; "yyy"; "3"])
