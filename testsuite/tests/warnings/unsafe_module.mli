[@@@ocaml.unsafe {|
  As you could guess, Unsafe_module is unsafe.
  Please use something else!
|} ]

module M: sig
  val x: int
    [@@ocaml.unsafe]

  type t
    [@@ocaml.unsafe]
end
[@@ocaml.unsafe]
