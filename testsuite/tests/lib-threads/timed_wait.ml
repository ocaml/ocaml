(* TEST
 include systhreads;
 hassysthreads;
 {
   native;
 }{
   bytecode;
 }
*)

module type S = sig
  type t
  val take : t -> unit
  val put : t -> int -> unit
end

module Make (M : sig
  type t
  type u
  val make : u -> t
  val timed_acquire : t -> float -> bool
  val release : t -> unit
end) = struct
  type t = {
    sem: M.t;
    mutable cell: int;
  }

  let take { sem; cell } =
    if M.timed_acquire sem 0.1
    then Format.printf "Read %d@." cell
    else Format.printf "Timed out@."

  let put t x =
    t.cell <- x;
    M.release t.sem

  let create init = { sem = M.make init; cell = 0 }
end

let run (type t) t m x =
  let module M = (val m : S with type t = t) in
  M.put t x;
  let d = Thread.create (fun _ ->
    M.take t;
    M.take t
  ) ()
  in
  Thread.join d

let _ =
  let module M = Make (struct
    include Semaphore.Binary
    type u = bool
  end) in
  let t = M.create false in
  run t (module M) 111;
  print_endline "Done Semaphore.Binary"

let _ =
  let module M = Make (struct
    include Semaphore.Counting
    type u = int
  end) in
  let t = M.create 0 in
  run t (module M) 222;
  print_endline "Done Semaphore.Counting"
