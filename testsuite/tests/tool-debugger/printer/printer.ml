type t = A | B

let print_t out t =
  Format.fprintf out "%s"
    (match t with
     | A -> "~A"
     | B -> "~B"
    )

(* A polymorphic type to test generic printers *)
type ('a, 'b) v = D of 'a * 'b

type 'a printer = Format.formatter -> 'a -> unit

let print_generic (type a b) (pa : a printer) (pb : b printer) : (a, b) v printer =
  fun ppf (D (a, b)) ->
  Format.fprintf ppf "D<%a, %a>"
    pa a
    pb b

let p : Format.formatter -> int -> unit = fun fmt n ->
  (* We use `max_printer_depth` to tweak the output so that
     this test shows that the printer not only compiles
     against the debugger's code, but also uses its state. *)
  for _i = 1 to min n !Ocamldebug.Printval.max_printer_depth do
    Format.pp_print_string fmt "S ";
  done;
  Format.pp_print_string fmt "O"
