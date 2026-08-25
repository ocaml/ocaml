(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module type T = sig
  val foo : int -> int
end

module H = struct
  let foo = succ
end

module F = struct
  let f (module S : T) g x : int =
    let y = g (module S) (x + 1) in
    y + 220.2

  let on_obj handler x =
    (handler # from (handler # deal x)) ^ handler # finalize

  let g =
    let x = f (module H) (fun (module H : T) x -> H.foo x) |> on_obj object
      end
    in
    on_obj object
      method deal = String.to_seq
      method from = String.of_seq
      method finalize = 44
    end (x ^ "foo")
end

let fgh (~a, ~b, ~c) = (a + b + (int_of_string c))

let u = fgh (1, 2, 3) + fgh (~a:11, ~b:300, ~c:3000) + fgh (~a:22, ~b:1, ~c:"30")

type r =
  | Foo of { bar: int; baz: string option}

let f = function
  | Foo {bar; _} -> (int_of_string bar) + 10

let g x = 10
