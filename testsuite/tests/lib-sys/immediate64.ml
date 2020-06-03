(* TEST
*)

module M : sig
  type t [@@immediate64]
  val zero : t
  val one : t
  val add : t -> t -> t
end = struct

  include Sys.Immediate64.Make(Int)(Int64)

  module type S = sig
    val zero : t
    val one : t
    val add : t -> t -> t
  end

  let impl : (module S) =
    match repr with
    | Immediate ->
        (module Int : S)
    | Non_immediate ->
        (module Int64 : S)

  include (val impl : S)
end

let () =
  match Sys.word_size with
  | 64 -> assert (Obj.is_int (Obj.repr M.zero))
  | _  -> assert (Obj.is_block (Obj.repr M.zero))
