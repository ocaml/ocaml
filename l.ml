let [@inline never] helper m x = Some (m, x)

let f m x =
  match helper m x with
  | None ->
    x
  | Some
      (z, 42) ->
    1 + z
  | Some
      (z, y) ->
    2 + z * y

let test1 f x =
  let a, b, c = f x in
  a + b * c

let () = ignore (Sys.opaque_identity (f 1 2))

(*
let f m x =
  match helper m x with
  | None -> x
  | Some (z, 42) -> 1 + z
  | Some (z, y) -> 2 + z * y
*)

(*
let consts x f g =
  match x with
  | 0 | 1 | 2 -> f x
  | 30 | 40 | 70 -> g x
  | 5 | 9 -> f x
  | 11 -> g x
  | _ -> x
*)
