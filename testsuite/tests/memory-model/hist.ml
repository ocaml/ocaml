(*****************************************************)
(* Histogram for collecting counts of final outcomes *)
(*****************************************************)

module type Key = sig
  type t
  val compare : t -> t -> int
  val ok : t -> bool
  val allowed : bool
  val name : string
  val pp : out_channel -> t -> unit
end

module Make(Cfg:sig val verbose : bool end)(K:Key) =
  struct

    module M = Map.Make(K)

    type key = K.t

    type t = int M.t

    let empty = M.empty

    let see k m =
      let old = try M.find k m with Not_found -> 0 in
      M.add k (old+1) m

    let pp chan m =
      if Cfg.verbose then begin
          Printf.fprintf chan "Test %s %s\n"
            K.name (if K.allowed then "Allowed" else "Forbidden") ;
          Printf.fprintf chan "Histogram (%d states)\n" (M.cardinal m) ;
        end ;
      let yes,no =
        M.fold
          (fun k n (yes,no) ->
            let ok = K.ok k in
            if Cfg.verbose then begin
                let c = if ok then '*' else ':' in
                Printf.fprintf chan "%8d%c> %a\n" n c K.pp k
              end ;
            if ok then (yes+n,no) else (yes,no+n))
          m (0,0) in
      let tag = match yes,no with
        | 0,_ -> "Never"
        | _,0 -> "Always"
        | _,_ -> "Sometimes" in
      if Cfg.verbose then begin
          Printf.fprintf chan
            "Observation %s %s %d %d\n" K.name tag yes no
        end else begin
          Printf.fprintf chan "Observation %s %s\n" K.name tag
        end ;
      if not K.allowed && yes <> 0 then begin
          Printf.fprintf chan "Invalid behaviour on test %s\n" K.name
        end ;
      if Cfg.verbose then Printf.fprintf chan "\n%!"

    let union = M.union (fun _ x y -> Some (x+y))
  end
