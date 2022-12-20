module type Root = sig
  type 'a t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val delete : 'a t -> unit
end

module Ref : Root = struct
  type 'a t = 'a ref
  let create v = ref v
  let get r = !r
  let delete _ = ()
end

module Genroot : Root = struct
  type 'a t
  external create : 'a -> 'a t         = "generational_ref_create"
  external get : 'a t -> 'a            = "generational_ref_get"
  external delete : 'a t -> unit       = "generational_ref_delete"
end

module Primes (Root : Root) : sig
  val count_upto : int -> int
end = struct
  let consume r =
    let v = Root.get r in
    Root.delete r; v
  
  type 'a rseq = Seq of (unit -> 'a rnode) [@@unboxed]
  
  and 'a rnode =
  | Nil
  | Cons of ('a * 'a rseq) Root.t
  
  let cons x xs = Cons (Root.create (x, xs))
  
  let numbers_from_to k n =
    let rec loop k n = Seq (fun () ->
      if k > n then Nil
      else cons k (loop (k + 1) n)
    ) in loop k n
  
  let rec map_consume f (Seq f) = Seq (fun () ->
    match f () with
    | Nil -> Nil
    | Cons r ->
      let (x, xs) = consume r in
      cons (f x) (map_consume f xs)
  )
  
  let rec filter_consume p (Seq f) = Seq (fun () ->
    match f () with
    | Nil -> Nil
    | Cons r ->
      let (x, xs) = consume r in
      let ys = filter_consume p xs in
      if p x then cons x ys else (let (Seq f) = ys in f ())
  )
  
  let seq_uncons_consume (Seq f) = match f () with
  | Nil -> None
  | Cons r -> Some (consume r)
  
  (* prime numbers up to [n] *)
  let rec eratosthenes n =
    let rec loop n (Seq f) = Seq (fun () ->
      match f () with
      | Nil -> Nil
      | Cons r as nums ->
         let (k, rest) = Root.get r in
         if k * k > n then nums
         else begin
           Root.delete r;
           cons k
             (rest
              |> filter_consume (fun i -> i mod k <> 0)
              |> loop n)
         end
    ) in
    loop n (numbers_from_to 2 n)

  let rec to_list_consume (Seq f) = match f () with
    | Nil -> []
    | Cons r ->
       let (x, xs) = consume r in
       x :: to_list_consume xs

  let rec length_consume (Seq f) = match f () with
    | Nil -> 0
    | Cons r ->
       let (_, xs) = consume r in
       1 + length_consume xs

  let count_upto n =
    eratosthenes n
    |> length_consume
end

module Conf = struct
  let n =
    try int_of_string (Sys.getenv "N")
    with _ ->
      Printf.ksprintf failwith "We expected an environment variable N with an integer value."
  
  let doms =
    try int_of_string (Sys.getenv "DOMS")
    with _ ->
      Printf.ksprintf failwith "We expected an environment variable DOMS with an integer value."

  type impl = Ref | Genroot
  let impl =
    let impls = [("ref", Ref); ("genroot", Genroot)] in
    try List.assoc (Sys.getenv "IMPL") impls
    with _ ->
      Printf.ksprintf failwith "We expected an environment variable IMPL, 'ref' or 'genroot'."

  let impl_mod : (module Root) = match impl with
    | Ref -> (module Ref)
    | Genroot -> (module Genroot)
  
  module Impl = (val impl_mod : Root)
end

module P = Primes(Conf.Impl)

let () =
  let do_work () = P.count_upto Conf.n in
  let worker_domains = List.init (Conf.doms - 1) (fun _ -> Domain.spawn do_work) in
  let res0 = do_work () in
  let results = res0 :: List.map Domain.join worker_domains in
  Printf.printf "%d\n%!" res0;
  assert (List.for_all ((=) res0) results);
