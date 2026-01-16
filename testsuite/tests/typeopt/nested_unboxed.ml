(* TEST
   flat-float-array;
   bytecode;
   native;
*)

(* Check that the behaviour of Typeopt functions is sound when
   dealing with unboxed types nested too deep *)

module type T = sig type t val mk : unit -> t end
module F (X : T) = struct type t = { x : X.t } [@@unboxed] let mk () = { x = X.mk () } end
module F10 (X : T) = F(F(F(F(F(F(F(F(F(F(X))))))))))
module F100 (X : T) = F10(F10(F10(F10(F10(F10(F10(F10(F10(F10(X))))))))))

module B = struct type t = float let mk () = 0. end
module M = F(F100(B))

(* M.t is 101 layers of unboxed wrappers around the type float.
   The following function checks that the runtime behaviour
   is consistent with that. *)

let run () =
  let x = M.mk () in
  let y = lazy x in
  (* x is represented by a float, so lazy values cannot be shortcut
     if we are in the default mode of using flat float arrays *)
  assert (Obj.tag (Obj.repr y) = Obj.forward_tag);

  let a = Array.make 10 y in
  assert (Obj.tag (Obj.repr a) <> Obj.double_array_tag);
  let z = Lazy.force a.(0) in
  assert (x = z)
