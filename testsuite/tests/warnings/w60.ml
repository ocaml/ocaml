(* TEST

flags = "-w +A-67"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

(* PR#7314 *)

module type Comparable = sig
  val id: int
end

module Make_graph (P:sig module Id:Comparable end) = struct
  let foo = P.Id.id
end

module Fold_ordered(P: sig module Id:Comparable end) =
struct
  include Make_graph(struct module Id = P.Id end)
end


(* PR#7314 *)

module M = struct
  module N = struct end
end

module O = M.N

(***************)

let () =
  (* M is unused, but no warning was emitted before 4.10. *)
  let module M = struct end in
  ()
