(* TEST
   * ocamldoc with html
*)

module M = Set.Make(struct
        type t = int
        let compare = compare
end)
