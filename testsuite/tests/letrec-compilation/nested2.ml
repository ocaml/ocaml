(* TEST *)

(* #13931 *)

let f a =
  let rec x =
    let rec y = Some a in y
  in x
