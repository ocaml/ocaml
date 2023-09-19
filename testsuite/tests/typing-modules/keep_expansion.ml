(* TEST
 expect;
*)

(* Found by Nicolas Ojeda Bar *)

module type S = sig
  type db
  type db_storage
end

module Make(X: S) = struct
  type db = X.db
  type db_storage = X.db_storage
end
[%%expect{|
module type S = sig type db type db_storage end
module Make :
  functor (X : S) -> sig type db = X.db type db_storage = X.db_storage end
|}]

module M = Make (struct type db = unit type db_storage = db end)
[%%expect{|
module M : sig type db = unit type db_storage = unit end
|}]
