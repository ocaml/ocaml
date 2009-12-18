(** Testing display of types.
   - {{!M}lien vers le module [M]}
   - {{!type:MT.t}lien vers le type [MT.t]}
   {!M}
   @test_types_display
 *)

let x = 1


module M = struct
  let y = 2

end

module type MT = sig
  type t = string -> int -> string -> (string * string * string) ->
    (string * string * string) ->
      (string * string * string) -> unit
  val y : int
end
