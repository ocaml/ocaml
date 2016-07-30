module M = struct
  type t = int

  let x = 10
end
[@@ocaml.unsafe]

let _ = M.x
include M
