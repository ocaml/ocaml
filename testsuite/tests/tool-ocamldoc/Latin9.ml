(* TEST
  ocamldoc with html;
*)

module Été = struct
 exception Éclair
end

exception Là

(** Exceptions and parameters must be in latin-9 subset of unicode.
    In the \@since version and in the description any character (e.g 字) is accepted:
   @since λ1
   @raise Là स्तनति
   @raise Été.Éclair þunor
   @param éponyme ?
*)
let f éponyme = if Random.int 2 > éponyme then raise Été.Éclair else raise Là
