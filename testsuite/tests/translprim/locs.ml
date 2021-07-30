(* TEST *)

let print_loc loc =
  print_endline loc

let print_file file =
  print_endline file

let print_line line =
  print_endline (Int.to_string line)

let print_module md =
  print_endline md

let print_pos (file, line, col1, col2) =
  Printf.printf "%s, %d, %d, %d\n" file line col1 col2

let () = print_loc __LOC__

let () = print_file __FILE__

let () = print_line __LINE__

let () = print_module __MODULE__

let () = print_pos __POS__

let loc, s1 = __LOC_OF__ "an expression"

let () = print_loc loc

let () = print_endline s1

let line, s2 = __LINE_OF__ "another expression"

let () = print_line line

let () = print_endline s2

let pos, s3 = __POS_OF__ "yet another expression"

let () = print_pos pos

let () = print_endline s3

let id x = Sys.opaque_identity x

let bang () = print_endline __FUNCTION__

let fn_multi _ _ = print_endline __FUNCTION__

let fn_function = function
  | f -> print_endline __FUNCTION__

let fn_poly : 'a . 'a -> unit = fun _ ->
  print_endline __FUNCTION__

module Mod1 = struct
  module Nested = struct
    let apply () = print_endline __FUNCTION__
  end
end

let anon () =
  print_endline __FUNCTION__;
  let fn = print_endline __FUNCTION__; id (fun () -> print_endline __FUNCTION__) in
  fn ()

let double_anon f =
  print_endline __FUNCTION__;
  let fn = id (fun () ->
    print_endline __FUNCTION__;
    let fn = id (fun () -> print_endline __FUNCTION__) in
    fn ()) in
  fn ()

let local () =
  print_endline __FUNCTION__;
  let inner () = print_endline __FUNCTION__ in
  (id inner) ()

let double_local () =
  print_endline __FUNCTION__;
  let inner1 () =
    print_endline __FUNCTION__;
    let inner2 () = print_endline __FUNCTION__ in
    (id inner2) () in
  (id inner1) ()

let local_no_arg =
  print_endline __FUNCTION__;
  let inner () = print_endline __FUNCTION__ in
  fun () -> print_endline __FUNCTION__; id inner ()

let curried () =
  print_endline __FUNCTION__;
  let inner () () = print_endline __FUNCTION__ in
  id (inner ())

let local_module () =
  print_endline __FUNCTION__;
  let module N = struct
    let foo () =
      print_endline __FUNCTION__
    let r = print_endline __FUNCTION__; ref ()
    let () = r := id (id foo ())
  end in
  !N.r

module Functor (X : sig end) = struct
  let fn () = print_endline __FUNCTION__
end
module Inst = Functor (struct end)

module rec Rec1 : sig
  val fn : unit -> unit
end = struct
  module M = Rec2 (struct end)
  let fn () = print_endline __FUNCTION__; M.fn ()
end
and Rec2 : functor (X : sig end) -> sig
  val fn : unit -> unit
end = functor (X : sig end) -> struct
  let fn () = print_endline __FUNCTION__
end

let (+@+) _ _ = print_endline __FUNCTION__

class klass = object (self)
  method meth () =
    print_endline __FUNCTION__
end

let inline_object () =
  let obj = object (self)
    method meth =
      print_endline __FUNCTION__;
      self#othermeth
    method othermeth =
      print_endline __FUNCTION__
  end in
  obj#meth

let () =
  fn_multi 1 1;
  fn_function ();
  fn_poly 42;
  Mod1.Nested.apply ();
  anon ();
  double_anon ();
  local ();
  double_local ();
  local_no_arg ();
  curried () ();
  local_module ();
  Inst.fn ();
  Rec1.fn ();
  42 +@+ 32;
  (new klass)#meth ();
  inline_object ();
  bang ()
