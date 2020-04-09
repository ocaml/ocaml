(* TEST
*)

type t = ..

module M = struct
  type t += A
  type t += B of int
end

type t += C
type t += D of int * string

let () =
  assert (Obj.Extension_constructor.of_val  M.A
          == [%extension_constructor M.A]);
  assert (Obj.Extension_constructor.of_val (M.B 42)
          == [%extension_constructor M.B]);
  assert (Obj.Extension_constructor.of_val  C
          == [%extension_constructor C]);
  assert (Obj.Extension_constructor.of_val (D (42, ""))
          == [%extension_constructor D])

let () = print_endline "OK"
