module Trans (M : Monad.S)
       : sig
  type 'a t = 'a option M.t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val lift : 'a M.t -> 'a t
end = struct
  type 'a t = 'a option M.t

  let bind m f =
    M.bind m @@
      function
      | Some x -> f x
      | None -> M.return None

  let return a = M.return (Some a)

  let lift m = M.map (fun a -> Some a) m
end

module Basic : Monad.Basic with type 'a t = 'a option = Trans(Identity)

module Made : Monad.S with type 'a t = 'a option = Monad.Make(Basic)

include Made
