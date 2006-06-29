type t 'a = [ Nil | Cons of 'a and t 'a ];

module A : sig
  value app_hd : t 'a -> ('a -> 'a) -> option 'a;
end = struct
  value app_hd x f =
    match x with
    [ Nil -> None
    | Cons x _ -> Some (f x) ];
end;
open A;

module M = struct
  external mk_nil : unit -> t 'a = "%identity";
  value nil = mk_nil ();
  (* value is_nil x = x = nil; *)
end;

(* M.app_hd succ (M.Cons 1 M.Nil); *)
(* M.hd (M.Cons 1 M.Nil); *)
app_hd (M.nil : t 'a) (fun (x : int) -> (x : 'a));
