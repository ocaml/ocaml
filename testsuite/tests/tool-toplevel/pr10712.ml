(* TEST
   * toplevel
   * toplevel.opt
*)

module A : sig
  type ('foo, 'bar) t

  val get_foo : ('foo, _) t -> 'foo option
end = struct
  type ('foo, 'bar) t =
    | Foo of 'foo
    | Bar of 'bar

  let get_foo = function
    | Foo foo -> Some foo
    | Bar _ -> None
end
;;

(* Type variables should be 'foo and 'a (name persists) *)
A.get_foo
;;

(* Type variables be 'a and 'b (original names lost in let-binding) *)
let _bar = A.get_foo
;;
