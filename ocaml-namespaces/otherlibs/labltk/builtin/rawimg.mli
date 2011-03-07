(*
 * Minimal pixmap support
 *)

type t
type pixel

val width : t -> int
    (* [width pixmap] *)
val height : t -> int
    (* [height pixmap] *)

val create : int -> int -> t
    (* [create width height] *)
val get : imagePhoto -> t
    (* [get img] *)
val set : imagePhoto -> t -> unit
    (* [set img pixmap] *)
val blit : imagePhoto -> t -> int -> int -> int -> int -> unit
    (* [blit img pixmap x y w h] (all ints must be non-negative) *)
val from_file : string -> t
    (* [from_file filename] *)

val copy : t -> t -> unit
    (* [copy src dst] *)

(*
 * Pixel operations
 *)
val get_pixel : t -> int -> int -> pixel
    (* [get_pixel pixmap x y] *)
val set_pixel : t -> int -> int -> pixel -> unit
    (* [set_pixel pixmap x y pixel] *)
val default_color : pixel

val pixel : int -> int -> int -> pixel
    (* [pixel r g b]   (r,g,b must be in [0..255]) *)

(*-*)
(* unsafe *)
val unsafe_copy : t -> t -> unit
val unsafe_get_pixel : t -> int -> int -> pixel
val unsafe_set_pixel : t -> int -> int -> pixel -> unit
(* /unsafe *)
