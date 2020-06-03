(* TEST
   flags = "-g"
   compare_programs = "false"
 *)


let id x = Sys.opaque_identity x

let[@inline never] bang () = raise Exit


let[@inline never] fn_multi _ _ f = f 42 + 1

let[@inline never] fn_function = function
  | f -> f 42 + 1

let[@inline never] fn_poly : 'a . 'a -> ('a -> int) -> int = fun x f ->
  f x + 1

module Mod1 = struct
  module Nested = struct
    let[@inline never] apply f = f 42 + 1
  end
end

let[@inline never] anon f =
  let fn = id (fun () -> f 42 + 1) in
  fn ()

let[@inline never] double_anon f =
  let fn = id (fun () ->
    let fn = id (fun () ->
      f 42 + 1) in
    fn ()) in
  fn ()

let[@inline never] local f =
  let[@inline never] inner () = f 42 + 1 in
  (id inner) () + 1

let[@inline never] double_local f =
  let inner1 () =
    let inner2 () = f 42 + 1 in
    (id inner2) () + 1 in
  (id inner1) () + 1

let local_no_arg =
  let inner f = f 42 + 1 in
  fun[@inline never] f -> (id inner) f + 1

let[@inline never] curried () =
  let inner () f = f 42 in
  id (inner ())

let[@inline never] local_module f =
  let module N = struct
    let[@inline never] foo () =
      f 42 + 1
    let r = ref 0    let () = r := id (id foo ())
  end in
  !N.r

module Functor (X : sig end) = struct
  let[@inline never] fn f = f 42 + 1
end
module Inst = Functor (struct end)

module rec Rec1 : sig
  val fn : (int -> int) -> int
end = struct
  module M = Rec2 (struct end)
  let[@inline never] fn f = M.fn f + 1
end
and Rec2 : functor (X : sig end) -> sig
  val fn : (int -> int) -> int
end = functor (X : sig end) -> struct
  let[@inline never] fn f = f 42 + 1
end

let[@inline never] (+@+) n f = f 42 + 1

class klass = object (self)
  val other = new klass2 "asdf"
  method meth f : int =
    other#othermeth 1 1 f 1 + 1
end
and klass2 _v = object (self)
  method othermeth _ _ f _ =
    (id (fun g -> g 42 + 1) f) + 1
end

let inline_object f =
  let obj = object (self)
    method meth : int =
      self#othermeth 1 f 1 + 1
    method othermeth _ _ _ =
      f 42 + 1
  end in
  obj#meth

let () =
  Printexc.record_backtrace true;
  match
    fn_multi 1 1 @@ fun _ ->
    fn_function @@ fun _ ->
    fn_poly 42 @@ fun _ ->
    Mod1.Nested.apply @@ fun _ ->
    anon @@ fun _ ->
    double_anon @@ fun _ ->
    local @@ fun _ ->
    double_local @@ fun _ ->
    local_no_arg @@ fun _ ->
    curried () @@ fun _ ->
    local_module @@ fun _ ->
    Inst.fn @@ fun _ ->
    Rec1.fn @@ fun _ ->
    42 +@+ fun _ ->
    (new klass)#meth @@ fun _ ->
    inline_object @@ fun _ ->
    bang ()
  with
  | _ -> assert false
  | exception Exit ->
     Printexc.print_backtrace stdout
