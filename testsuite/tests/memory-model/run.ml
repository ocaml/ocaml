(****************)
(* Run one test *)
(****************)

module type Test = sig

  module Key : Hist.Key

  module Env : Shared.S

  type out0
  val code0 : Env.in_t -> out0

  type out1
  val code1 : Env.in_t -> out1

  val out2key : Env.in_t -> out0 -> out1 -> Key.t
end


module Make(C:Opt.Config)(T:Test) =
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
            (* Initialise memory and barrier *)
            T.Env.reinit env ;
            Barrier.reinit barrier ;
            (* Perform on test round *)
            let d0 = Domain.spawn f0 in
            let d1 = Domain.spawn f1 in
            let t0 = Domain.join d0 in
            let t1 = Domain.join d1 in
            (* Collect outcome *)
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

    let  ninstances = max 1 (C.navail / 2)

    let zyva () =
      List.init ninstances
        (fun _ ->
          let module I = Instance(struct end) in
          Domain.spawn I.zyva)
      |> List.map Domain.join
      |> List.fold_left Hist.union Hist.empty
      |> Hist.pp stdout
  end
