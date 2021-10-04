(* TEST
   flags="-w +48"
*)

external ( @@ ) :  ('a -> 'b) -> 'a -> 'b = "%apply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      f @@ 3; (* 6 *)
      g @@ f @@ 3; (* 36 *)
      f @@ g @@ 3; (* 18 *)
      h @@ g @@ f @@ 3; (* 37 *)
      add 4 @@ g @@ f @@ add 3 @@ add 2 @@ 3; (* 260 *)
    ]
external ( @@ ) :  ('a -> 'b) -> 'a -> 'b = "%apply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      f @@ 3; (* 6 *)
      g @@ f @@ 3; (* 36 *)
      f @@ g @@ 3; (* 18 *)
      h @@ g @@ f @@ 3; (* 37 *)
      add 4 @@ g @@ f @@ add 3 @@ add 2 @@ 3; (* 260 *)
    ]

(* PR#10081 *)
let bump ?(cap = 100) x = min cap (x + 1)
let _f x = bump @@ x (* no warning 48 *)

(* Abstract functions *)
let _ =
  let module A:sig
    type f
    type x
    val succ: f
    val zero:x
    external (@@): f -> x -> int = "%apply"
  end = struct
    type f = int -> int
    type x = int
    let succ = succ
    let zero = 0
    external (@@): f -> x -> int = "%apply"
  end in
  A.(succ @@ zero)
