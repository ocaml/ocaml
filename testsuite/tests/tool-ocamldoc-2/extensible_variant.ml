(** Testing display of extensible variant types.

   @test_types_display
 *)

type e = ..

module M = struct
  type e +=
  | A (** A doc *)
  | B (** B doc *)
  | C (** C doc *)
end

module type MT = sig
  type e +=
  | A (** A doc *)
  | B (** B doc *)
  | C (** C doc *)
end
