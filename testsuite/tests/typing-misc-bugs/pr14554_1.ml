(* TEST_BELOW *)

(* see typing-misc/letitem.ml for more context *)
type 'a t

let dog : 'this =
  let module Dog = struct
    external make
      : bark:('self -> unit)
      -> < bark : ('self -> unit) > t = "%identity"
  end
  in
  Dog.make ~bark:(fun (o : 'this) -> ())

(* TEST
 flags = "-stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
