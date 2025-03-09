module rec M : sig
  val f : unit -> unit
  val g : unit -> unit
end = struct
  let rec f () = () and g () = ()
end

let () = print_endline "ok"
