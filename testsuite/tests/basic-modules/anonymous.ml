(* TEST_BELOW
(* Blank lines added here to preserve locations. *)














*)

module _ = struct
  let x = 13, 37
end

module rec A : sig
  type t = B.t
end = A
and _ : sig
  type t = A.t
  val x : int * int
end = struct
  type t = B.t
  let x = 4, 2
end
and B : sig
  type t
end = struct
  type t

  let x = "foo", "bar"
end

module type S

let f (module _ : S) = ()

type re = { mutable cell : string; }

let s = { cell = "" }

module _ = struct
 let () = s.cell <- "Hello World!"
end

let drop _ = ()

let () = drop s.cell

(* TEST
 flags = "-c -nostdlib -nopervasives -dlambda -dno-unique-ids";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/anonymous.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   {
     no-flambda;
     compiler_reference = "${test_source_directory}/anonymous.ocamlopt.reference";
     check-ocamlopt.byte-output;
   }{
     flambda;
     compiler_reference = "${test_source_directory}/anonymous.ocamlopt.flambda.reference";
     check-ocamlc.byte-output;
   }
 }
*)
