(* TEST
   flags="-w +48"
*)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      3 |> f; (* 6 *)
      3 |> f |> g; (* 36 *)
      3 |> g |> f; (* 18 *)
      3 |> f |> g |> h; (* 37 *)
      3 |> add 2 |> add 3 |> f |> g |> add 4; (* 260 *)
    ]


(* PR#10081 *)
let bump ?(cap = 100) x = min cap (x + 1)
let _f x = x |> bump (* no warning 48 *)

(* PR#10081 *)
type t = A | B
type s = A | B
let _f (x : t) = x |> function A -> 0 | B -> 1
