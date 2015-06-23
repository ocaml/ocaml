module Option : sig
  type 'a t = 'a option
  val is_some : 'a t -> bool
end = struct
  type 'a t = 'a option
  let is_some = function
    | None -> false
    | Some _ -> true
end

let test ?x () = Option.is_some x
