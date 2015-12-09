exception Exit

let r = ref ""

let guarded f =
  match f () with
  | true | exception Exit when r := "hello"; true -> !r
  | _ -> "other"
;;
