(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "link_intf_impl.mli link_intf_impl.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -uid-deps link_intf_impl.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

let x (* 0 *) = 42

type t (* 1 *) = int

module type S (* 3 *) = sig
  val y (* 2 *) : t
end

module M (* 5 *) : S = struct
  let y (* 4 *) = 36
end

module N (* 8 *) : sig
  val y (* 7 *) : int
end = struct
  let y (* 6 *) = 2
end

let _ = (module N : S)

module P (* 10 *)= struct
  let y (* 9 *) = 12
end

module F (* 12 *) (X (* 11 *) : S) = X

module G (* 13 *) = F(P)

module type Initial (* 16 *) = sig
  module type Nested (* 15 *) = sig
    type t (* 14 *)
  end
end

module MF (* 23 *) : sig
   module F (* 22 *) (X (* 21 *) : sig val x (* 20 *) : int end) : sig end
end = struct
  module F (* 19 *) (X (* 18 *) : sig val x (* 17 *) : int end) = struct end
end

module FMT (* 27 *) (X (* 26 *) : sig
  module type MT (* 25 *) = sig val x (* 24 *) : int end
end) : sig end = struct end
