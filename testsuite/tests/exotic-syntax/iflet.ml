
let ok() = print_endline "ok";;
let fail() = print_endline "fail";;

let assert_ b = if b then ok() else fail();;

if let Some x = None then fail() else ok();;

if let Some x = Some 42 then assert_ (x=42) else fail ();;

if let [1;2;y] = [1;2;3] then assert_ (y=3) else fail ();;

if let [x] = [1] and [y] = [2] then assert_ (x=1 && y=2) ;;

type foo =
  | A of int
  | B of int * string
  | C
;;

let f foo =
  if let A x | B (x, _) = foo then ok ();;

f (A 42);;
f (B (0, "foo"));;
