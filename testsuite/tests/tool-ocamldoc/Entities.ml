(* TEST
   * ocamldoc with html
*)

type ul
type li
type amp
type 'a t = <ul: <li:[<`A of &amp] as 'a> >
