(* TEST
   * toplevel
*)

(* simple printer *)
type t = A | B;;

let print_t out t =
  Format.fprintf out "%s"
    (match t with
     | A -> "~A"
     | B -> "~B"
    );;

#install_printer print_t;;
type u = C of t;;
B;;
C B;;


(* old-style printer *)
type old = OA | OB;;
let print_old = function
  | OA -> Format.printf "Old(A)"
  | OB -> Format.printf "Old(B)"
;;

#install_printer print_old;;
[OA; OB];;


(* genreic printers *)
type ('a, 'b) v = D of 'a * 'b;;

type 'a printer = Format.formatter -> 'a -> unit;;
let print_generic (type a b) (pa : a printer) (pb : b printer) : (a, b) v printer =
  fun ppf (D (a, b)) ->
  Format.fprintf ppf "D<%a, %a>"
    pa a
    pb b
;;

#install_printer print_generic;;
[D (0, A); D (42, B)];;
