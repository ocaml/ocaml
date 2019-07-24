(* TEST

flags = "-w A-27-32"

*)

type t =
  { foo: int }

let foo = 12

module L = struct
  type t =
    { foo : int }

  let foo = 34
end

let _ = {foo = foo}

let _ = {L.foo = foo}

let _ = {L.foo = L.foo}

let _ = {foo}

let _ = {L.foo}

let {foo = foo} = {foo = 12}

let {foo} = {foo = 13}

let {L.foo = foo} = {L.foo = 43}

let f ~foo () = ()

let g ?foo () = ()

let _ = f ~foo:foo ()

let foo = Some 12

let _ = g ?foo:foo ()

let _ = f ~foo:L.foo ()

module M = struct
  let foo = Some 32
end

let _ = g ?foo:M.foo ()

let f ~foo:foo () = ()

let g ?foo:foo () = ()

let _ =
  object
    val x = 12
    method f x = {< x = x >}
  end
