(* TEST
  modules="opt.ml barrier.ml hist.ml shared.ml run.ml outcome.ml"
  * not-bsd
  ** bytecode
  ** native
*)

(* Memory model test:
 *   Check that all basic forbidden tests are not observed
 *)


module O = Opt.Make(struct let pgm = "forbidden"end)
module Config = O.Config
let verbose = Config.verbose

(********************************************************************)
(* Shared memory, locations x and y of type int ref or int Atomic.t *)
(********************************************************************)

module IntRef = struct
  type t = int ref
  let empty _ = ref (-1)
  let reinit r = r := 0
end

module IntAto = struct
  type t = int Atomic.t
  let empty _ = Atomic.make (-1)
  let reinit r = Atomic.set r 0
end


module EnvPP = Shared.Make(IntRef)(IntRef)
module EnvPA = Shared.Make(IntRef)(IntAto)
module EnvAP = Shared.Make(IntAto)(IntRef)
module EnvAA = Shared.Make(IntAto)(IntAto)

(* Outcome make of two integers *)
module OK = Outcome.OK
module NO = Outcome.NO

module IntT = Outcome.Int

module Outcome = Outcome.Make(IntT)(IntT)


(* Store buffer *)

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

  module PFetch = struct

    module Key = MakeKey(struct let name = "MP+PFetch" end)(NO)

    module Env = EnvPA

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      ignore (Atomic.fetch_and_add y 1)

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = Atomic.get y in
      let r1 = !x in
      {t0=r0; t1=r1;}

    let out2key _ () { t0; t1; } = Key.make t0 t1

  end

  module PCas = struct

    module Key = MakeKey(struct let name = "MP+PCas" end)(NO)

    module Env = EnvPA

    type out0 = unit

    let code0 (x,y) =
      x := 1 ;
      while not (Atomic.compare_and_set y 0 1) do () done

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = Atomic.get y in
      let r1 = !x in
      {t0=r0; t1=r1;}

    let out2key (_,y) () { t0; t1; } =  assert (Atomic.get y = 1) ; Key.make t0 t1

  end

  module AP = struct

    module Key = MakeKey(struct let name = "MP+AP" end)(NO)

    module Env = EnvAP

    type in_t = int Atomic.t * int ref

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

  module FetchP = struct

    module Key = MakeKey(struct let name = "MP+FetchP" end)(NO)

    module Env = EnvAP

    type in_t = int Atomic.t * int ref

    type out0 = unit

    let code0 (x,y) =
      ignore (Atomic.fetch_and_add x 1) ;
      y := 1

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = !y in
      let r1 = Atomic.get x in
      {t0=r0; t1=r1;}

    let out2key (x,_) () { t0; t1; } = Key.make t0 t1

  end

  module CasP = struct

    module Key = MakeKey(struct let name = "MP+CasP" end)(NO)

    module Env = EnvAP

    type in_t = int Atomic.t * int ref

    type out0 = unit

    let code0 (x,y) =
      while not (Atomic.compare_and_set x 0 1) do () done ;
      y := 1

    type out1 = { t0:int; t1:int; }

    let code1 (x,y) =
      let r0 = !y in
      let r1 = Atomic.get x in
      {t0=r0; t1=r1;}

    let out2key (x,_) () { t0; t1; } = assert (Atomic.get x = 1) ; Key.make t0 t1

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

    let out2key _ () { t0; t1; } = Key.make t0 t1

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

module Forbid(C:Opt.Config) = struct
  module Run = Run.Make(C)
  module T1 = Run(SB.AA)
  let () = T1.zyva()
  module T2 = Run(R.PA)
  let () = T2.zyva()
  module T3 = Run(R.AA)
  let () = T3.zyva()
  module T4 = Run(MP.PA)
  let () = T4.zyva()
  module T40 = Run(MP.PFetch)
  let () = T40.zyva()
  module T41 = Run(MP.PCas)
  let () = T41.zyva()
  module T5 = Run(MP.AP)
  let () = T5.zyva()
  module T50 = Run(MP.FetchP)
  let () = T50.zyva()
  module T51 = Run(MP.CasP)
  let () = T51.zyva()
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

module Allow(C:Opt.Config) = struct
  module Run = Run.Make(C)
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
  if not verbose then begin
    let module Twice(C:Opt.Config) =
      struct
        module _ = Forbid(C)
        module _ =
          Forbid
            (struct
              let verbose = C.verbose
              let size = C.size / 10
              let nruns = C.nruns * 10
              let navail = C.navail
            end)
      end in
      let module _ = Twice(Config) in ()
  end else begin
    let module _ = Forbid(Config) in
    let module _ = Allow(Config) in
    ()
  end
