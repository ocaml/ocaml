(* TEST
flags = "-i -w +63"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

type 'a t = B of 'a t list

let rec foo f = function
  | B(v)::tl -> B(foo f v)::foo f tl
  | [] -> []

module DT = struct
  type 'a t = {bar : 'a}
  let p t = foo (fun x -> x) t
end
