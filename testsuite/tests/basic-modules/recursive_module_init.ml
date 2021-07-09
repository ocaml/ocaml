(* TEST *)

let check ~stub txt f =
  let run mode f =
    match f mode with
    | n -> string_of_int n
    | exception Undefined_recursive_module _ -> "__" in
  Printf.printf "%5s[%s]: nonrec => %s, self => %s, mod => %s\n%!"
    txt
    (if f == stub then "stub" else "real")
    (run `Nonrec f)
    (run `Self f)
    (run `Mod f)

module rec M : sig
  val f1 : [`Nonrec|`Self|`Mod] -> int
  val f2 : [`Nonrec|`Self|`Mod] -> int
  val f3 : [`Nonrec|`Self|`Mod] -> int
  val f4 : unit -> [`Nonrec|`Self|`Mod] -> int
  val f5 : unit -> [`Nonrec|`Self|`Mod] -> int
end = struct
  let rec f1 mode =
    match mode with
    | `Nonrec -> 42
    | `Self -> f1 `Nonrec
    | `Mod -> M.f1 `Nonrec
  let f2 = f1
  let f3 = M.f1
  let f4 () = f1
  let f5 () = M.f1

  let () =
    check ~stub:f3 "f1" f1;
    check ~stub:f3 "f2" f2;
    check ~stub:f3 "f3" f3;
    check ~stub:f3 "f4" (f4 ());
    check ~stub:f3 "f5" (f5 ())
end

let () =
  check ~stub:M.f3 "M.f1" M.f1;
  check ~stub:M.f3 "M.f2" M.f2;
  check ~stub:M.f3 "M.f3" M.f3;
  check ~stub:M.f3 "M.f4" (M.f4 ());
  check ~stub:M.f3 "M.f5" (M.f5 ())


module rec Foo : sig
  class cls : object
    method go : unit
  end
  module M : sig
    val foo : unit -> cls
    val bar : cls Lazy.t
  end
end = struct
  class cls = object
    method go = print_endline "go"
  end
  module M = struct
    let foo () = new Foo.cls
    let bar = lazy (foo ())
  end
end

let () =
  List.iter (fun x -> x#go)
    [new Foo.cls; Foo.M.foo(); Lazy.force Foo.M.bar]
