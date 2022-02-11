(* TEST
*)

(* Memory model test:
 *   Check that all basic forbidden tests are not observed
 *)



(* Configuration:
 * the test can also be used as a program, with command line options.
 * In particular, options `-v` also runs allowed tests,
 * whose occurence is legal according to the memory model
 * By contrast, option `-q` (default) does not runs
 * forbidden tests only, whose occurrence are not legal
 * according to the memory model.
*)

module type Config = sig
  val verbose : bool
  val size : int
  val nruns : int
  val ninstances : int
end

let verbose = ref false
and size = ref 5000
and nruns = ref 20
and ninstances = ref 1

let pgm = if Array.length Sys.argv > 0 then Sys.argv.(0) else "forbidden"

let () =
  let doc_def msg r = Printf.sprintf "<n> %s, default %d" msg !r in
  let open Arg in
  Arg.parse
    [
      "-v",Set verbose,"be verbose";
      "-q",Clear verbose,"be silent";
      "-s",Int (fun i -> size :=i), doc_def "size of arrays" size ;
      "-r",Int (fun i -> nruns :=i),doc_def "number of interations" nruns ;
      "-n",Int (fun i -> ninstances :=i),
      doc_def "number of concurrent test instances" ninstances ;
    ]
    (fun _ -> raise (Bad "No argument allowed"))
    (Printf.sprintf "%s: (options)*, where options are:" pgm)

module Config = struct
  let verbose = !verbose
  let size = !size
  let nruns = !nruns
  let ninstances = !ninstances
end

(**********************************************)
(* Simple, work once, synchronisation barrier *)
(**********************************************)

module Barrier = struct

type t = int Atomic.t array

let make sz = Array.init sz (fun _ -> Atomic.make 0)

let reinit t =
  for k=0 to Array.length t-1 do
    Atomic.set t.(k) 0
  done

let wait t id i =
  if i mod 2 = id then
    Atomic.set t.(i) 1
  else
    while Atomic.get t.(i) != 1 do
      Domain.cpu_relax ()
    done

end

(*****************************************)
(* Shared memory locations x and y,      *)
(* with all plain vs. atomic variations. *)
(*****************************************)

module type T = sig
  type t
  val empty : 'a -> t
  val set :  t -> int -> unit
end

module IntRef = struct
  type t = int ref
  let empty _ = ref (-1)
  let set r v = r := v
end

module IntAto = struct
  type t = int Atomic.t
  let empty _ = Atomic.make (-1)
  let set = Atomic.set
end

module MakeEnv(T0:T)(T1:T) = struct

  type t0 = T0.t
  type t1 = T1.t
  type t =
    {
      x :T0.t array ;
      y :T1.t array ;
      sz : int ;
    }

  let make sz =
    {
      x = Array.init sz T0.empty ;
      y = Array.init sz T1.empty ;
      sz;
    }

  let reinit env =
    let x = env.x and y = env.y in
    for i = 0 to env.sz-1 do
      T0.set x.(i) 0 ; T1.set y.(i) 0
    done

  let env2in env i = env.x.(i),env.y.(i)

end

module EnvPP = MakeEnv(IntRef)(IntRef)
module EnvPA = MakeEnv(IntRef)(IntAto)
module EnvAP = MakeEnv(IntAto)(IntRef)
module EnvAA = MakeEnv(IntAto)(IntAto)

(*********************************************)
(* Outcomes, _i.e._ final values of test run *)
(*********************************************)

(* Two integers *)

module
  Outcome
    (N:
       sig
         val name : string
         val tag0 : string
         val tag1 : string
         val ok : int -> int -> bool
       end)
    (A:sig val allowed:bool end) =
  struct

    type t = { r0 : int ; r1 : int; }

  let compare k1 k2 = match compare k1.r0 k2.r0 with
    | 0 -> compare k1.r1 k2.r1
    | r -> r

  let ok k = N.ok k.r0 k.r1

  let allowed = A.allowed

  let name = N.name

  let make r0 r1 = { r0; r1; }

  let pp chan k = Printf.fprintf chan "%s=%d; %s=%d;" N.tag0 k.r0 N.tag1 k.r1

end

module OK = struct let allowed = true end
module NO = struct let allowed = false  end

(*****************************************************)
(* Histogram for collecting counts of final outcomes *)
(*****************************************************)

module Hist = struct
  module type Key = sig
    type t
    val compare : t -> t -> int
    val ok : t -> bool
    val allowed : bool
    val name : string
    val pp : out_channel -> t -> unit
  end

  module type S = sig
    type t
    type key
    val empty : t
    val see : key -> t -> t
    val pp : out_channel -> t -> unit
    val union : t -> t -> t
  end

  module Make(Cfg:sig val verbose : bool end)(K:Key) : S with type key = K.t =
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
end

(****************)
(* Run one test *)
(****************)

module type Test = sig
  module Key : Hist.Key

  module Env : sig
    type t0
    type t1
    type t
    val make : int -> t
    val reinit : t -> unit
    val env2in : t -> int -> (t0 * t1)
  end

  type out0
  val code0 : (Env.t0 * Env.t1) -> out0

  type out1
  val code1 : (Env.t0 * Env.t1) -> out1

  val out2key : (Env.t0 * Env.t1) -> out0 -> out1 -> Key.t
end


module MakeRun(C:Config)(T:Test) =
  struct

    module Hist = Hist.Make(C)(T.Key)

    module Instance(_:sig end) = struct (* Applicative .. *)

      let env = T.Env.make C.size
      and barrier = Barrier.make C.size

      let mkf id code () =
        Array.init C.size
          (fun i ->
            Barrier.wait barrier id i ;
            code (T.Env.env2in env i))

      let f0 = mkf 0 T.code0
      and f1 = mkf 1 T.code1

      let zyva () =

        let rec run_rec hist j =
          if j >= C.nruns then hist
          else begin
            T.Env.reinit env ;
            Barrier.reinit barrier ;
            let d0 = Domain.spawn f0 in
            let d1 = Domain.spawn f1 in
            let t0 = Domain.join d0 in
            let t1 = Domain.join d1 in

            let rec hist_rec hist i =
              if i >= C.size then hist
              else
                let k = T.out2key (T.Env.env2in env i) t0.(i) t1.(i) in
                hist_rec (Hist.see k hist) (i+1) in
            let hist = hist_rec hist 0 in
            run_rec hist (j+1)
          end in
        run_rec Hist.empty 0
    end

    let zyva () =
      List.init C.ninstances
        (fun _ ->
          let module I = Instance(struct end) in
          Domain.spawn I.zyva)
      |> List.map Domain.join
      |> List.fold_left Hist.union Hist.empty
      |> Hist.pp stdout
  end

(************************)
(* And now tests.       *)
(************************)

module SB = struct

  module MakeKey(N:sig val name : string end) =
    Outcome
      (struct
        let name = N.name
        let tag0 = "r0"
        let tag1 = "r1"
        let ok r0 r1 = r0=0 && r1=0
      end)

  module PP = struct

    module Key = MakeKey(struct let name = "SB" end)(OK)

    module Env = EnvPP

    type out0 = int

    let code0 (x,y) =
      x  := 1 ;
      !y

    type out1 = int

    let code1 (x,y) =
      y := 1 ;
      !x

    let out2key _ r0 r1 = Key.make r0 r1

  end

  module PA = struct

    module Key = MakeKey(struct let name = "SB+PA" end)(OK)

    module Env = EnvPA

    type out0 = int

    let code0 (x,y) =
      x  := 1 ;
      Atomic.get y

    type out1 = int

    let code1 (x,y) =
      Atomic.set y 1 ;
      !x

    let out2key _ r0 r1 = Key.make r0 r1

  end

  module AA = struct

    module Key = MakeKey(struct let name = "SB+AA" end)(NO)

    module Env = EnvAA

    type out0 = int

    let code0 (x,y) =
      Atomic.set x  1 ;
      Atomic.get y

    type out1 = int

    let code1 (x,y) =
      Atomic.set y 1 ;
      Atomic.get x

    let out2key _ r0 r1 = Key.make r0 r1

  end
end

module R = struct

  module MakeKey(N:sig val name : string end) =
    Outcome
      (struct
        let name = N.name
        let tag0 = "y"
        let tag1 = "r0"
        let ok y r0 = y = 2 && r0 = 0
      end)

  module PP = struct

    module Key = MakeKey(struct let name = "R" end)(OK)

    module Env = EnvPP

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      y := 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      y := 2 ;
      !x

    let out2key (x,y) () r1 = Key.make !y r1

  end

  module PA = struct

    module Key = MakeKey(struct let name = "R+PA" end)(NO)

    module Env = EnvPA

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      Atomic.set y 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      Atomic.set y 2 ;
    !x

    let out2key (x,y) () r1 = Key.make (Atomic.get y) r1

  end

  module AP = struct

    module Key = MakeKey(struct let name = "R+AP" end)(OK)

    module Env = EnvAP

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 1 ;
      y := 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      y := 2 ;
      Atomic.get x

    let out2key (x,y) () r1 = Key.make !y r1

  end

  module AA = struct

    module Key = MakeKey(struct let name = "R+AA" end)(NO)

    module Env = EnvAA

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 1 ;
      Atomic.set y 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      Atomic.set y 2 ;
      Atomic.get x

    let out2key (x,y) () r1 = Key.make (Atomic.get y) r1

  end

end

module MP = struct

  module MakeKey(N:sig val name : string end) =
    Outcome
      (struct
        let name = N.name
        let tag0 = "r0"
        let tag1 = "r1"
        let ok r0 r1 = r0=1 && r1=0
      end)


  module PP = struct

    module Key = MakeKey(struct let name = "MP" end)(OK)

    module Env = EnvPP

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      y := 1

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = !y in
      let r1 = !x in
      {t0=r0; t1=r1;}

    let out2key _ () { t0; t1; } = Key.make t0 t1

  end

  module PA = struct

    module Key = MakeKey(struct let name = "MP+PA" end)(NO)

    module Env = EnvPA

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      Atomic.set y 1

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = Atomic.get y in
      let r1 = !x in
      {t0=r0; t1=r1;}

    let out2key _ () { t0; t1; } = Key.make t0 t1

  end

  module AP = struct

    module Key = MakeKey(struct let name = "MP+AP" end)(NO)

    module Env = EnvAP

    type in_t = int Atomic.t * int ref
    let env2in env i = env.Env.x.(i),env.Env.y.(i)

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 1 ;
      y := 1

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = !y in
      let r1 = Atomic.get x in
      {t0=r0; t1=r1;}

    let out2key _ () { t0; t1; } = Key.make t0 t1

  end

  module AA = struct

    module Key = MakeKey(struct let name = "MP+AA" end)(NO)

    module Env = EnvAA

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x  1 ;
      Atomic.set y  1 ;
      ()

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = Atomic.get y in
      let r1 = Atomic.get x in
      {t0=r0; t1=r1;}

    let out2key _ () { t0; t1; } = { Key.r0=t0; r1=t1; }

  end
end

module LB = struct
  module Key =
    Outcome
      (struct
        let name = "LB"
        let tag0 = "r0"
        let tag1 = "r1"
        let ok r0 r1 = r0=1 && r1=1
      end)(NO)

  module Env = EnvPP

  type out0 = int

  let code0 (x,y) =
    let r0 = !x in
    y := 1 ;
    r0

  type out1 = int

  let code1 (x,y) =
    let r = !y in
    x := 1 ;
    r

  let out2key _ r0 r1 = Key.make r0 r1

end

module W2 = struct

  module MakeKey(N:sig val name : string end) =
    Outcome
      (struct
        let name = N.name
        let tag0 = "x"
        let tag1 = "y"
        let ok x y = x=2 && y=2
      end)

  module PP = struct

    module Key = MakeKey(struct let name = "2+2W" end)(OK)

    module Env = EnvPP

    open Env

    type out0 = unit

    let code0 (x,y) =
      x := 2 ;
      y := 1

    type out1 = unit

    let code1 (x,y) =
      y := 2 ;
      x := 1

    let out2key (x,y) _ _ = Key.make !x !y

  end

  module PA = struct

    module Key = MakeKey(struct let name = "2+2W+PA" end)(NO)

    module Env = EnvPA

    open Env

    type out0 = unit

    let code0 (x,y) =
      x := 2 ;
      Atomic.set y 1

    type out1 = unit

    let code1 (x,y) =
      Atomic.set y 2 ;
      x := 1

    let out2key (x,y) _ _ = Key.make !x (Atomic.get y)

  end

  module AA = struct

    module Key = MakeKey(struct let name = "2+2W+AA" end)(NO)

    module Env = EnvAA

    open Env

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 2 ;
      Atomic.set y 1

    type out1 = unit

    let code1 (x,y) =
      Atomic.set y 2 ;
      Atomic.set x 1

    let out2key (x,y) _ _ = Key.make (Atomic.get x) (Atomic.get y)

  end

end

module S = struct

  module MakeKey(N:sig val name : string end) =
    Outcome
      (struct
        let name = N.name
        let tag0 = "x"
        let tag1 = "r0"
        let ok x r0 = x=2 && r0=1
      end)

  module PP = struct

    module Key = MakeKey(struct let name = "S" end)(OK)

    module Env = EnvPP

    type out0 = unit

    let code0 (x,y) =
      x := 2 ;
      y := 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      let r0 = !y in
      x := 1 ;
      r0

    let out2key (x,_) () r0 = Key.make !x r0

  end

  module PA = struct

    module Key = MakeKey(struct let name = "S+PA" end)(NO)

    module Env = EnvPA

    type out0 = unit

    let code0 (x,y) =
      x := 2 ;
      Atomic.set y 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      let r0 = Atomic.get y in
      x := 1 ;
      r0

    let out2key (x,_) () r0 = Key.make !x r0

  end

  module AP = struct

    module Key = MakeKey(struct let name = "S+AP" end)(NO)

    module Env = EnvAP

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 2 ;
      y := 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      let r0 = !y in
      Atomic.set x 1 ;
      r0

    let out2key (x,_) () r0 = Key.make (Atomic.get x) r0

  end

  module AA = struct

    module Key = MakeKey(struct let name = "S+AA" end)(NO)

    module Env = EnvAA

    type out0 = unit

    let code0 (x,y) =
      Atomic.set x 2 ;
      Atomic.set y 1 ;
      ()

    type out1 = int

    let code1 (x,y) =
      let r0 = Atomic.get y in
      Atomic.set x 1 ;
      r0

    let out2key (x,_) () r0 = Key.make (Atomic.get x) r0

  end


end

module Forbid(C:Config) = struct
  module Run = MakeRun(C)
  module T1 = Run(SB.AA)
  let () = T1.zyva()
  module T2 = Run(R.PA)
  let () = T2.zyva()
  module T3 = Run(R.AA)
  let () = T3.zyva()
  module T4 = Run(MP.PA)
  let () = T4.zyva()
  module T5 = Run(MP.AP)
  let () = T5.zyva()
  module T6 = Run(MP.AA)
  let () = T6.zyva()
  module T7 = Run(LB)
  let () = T7.zyva()
  module T8 = Run(W2.PA)
  let () = T8.zyva()
  module T9 = Run(W2.AA)
  let () = T9.zyva()
  module TA = Run(S.PA)
  let () = TA.zyva()
  module TB = Run(S.AP)
  let () = TB.zyva()
  module TC = Run(S.AA)
  let () = TC.zyva()
end

module Allow(C:Config) = struct
  module Run = MakeRun(C)
  module T1 = Run(SB.PP)
  let () = T1.zyva()
  module T2 = Run(SB.PA)
  let () = T2.zyva()
  module T3 = Run(R.PP)
  let () = T3.zyva()
  module T4 = Run(R.AP)
  let () = T4.zyva()
  module T5 = Run(MP.PP)
  let () = T5.zyva()
  module T6 = Run(W2.PP)
  let () = T6.zyva()
  module T7 = Run(S.PP)
  let () = T7.zyva()
end

let () =
  if not !verbose then begin
    let module Twice(C:Config) =
      struct
        module _ = Forbid(C)
        module _ =
          Forbid
            (struct
              let verbose = C.verbose
              let size = C.size / 10
              let nruns = C.nruns * 10
              let ninstances = C.ninstances
            end)
      end in
      let module _ = Twice(Config) in ()
  end else begin
      let module Zyva(C:Config) =
        struct
          module _ = Forbid(C)
          module _ = Allow(C)
        end in
      let module _ = Zyva(Config) in ()
    end
