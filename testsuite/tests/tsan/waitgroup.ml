type t

external create : int -> t = "wg_create" [@@noalloc]
external finish : t -> unit = "wg_finish" [@@noalloc]
external wait : t -> unit = "wg_wait" [@@noalloc]

let [@inline never] join t = finish t; wait t
