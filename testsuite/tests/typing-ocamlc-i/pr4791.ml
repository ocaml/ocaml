(* TEST_BELOW




*)

type t = A
module B =
struct
  type t = B
  let f A = B
end

(* TEST
{
  flags = "-i -w +63";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
