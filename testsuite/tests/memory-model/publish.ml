(* TEST
  modules="opt.ml barrier.ml hist.ml shared.ml run.ml outcome.ml"
  * not-bsd
  ** bytecode
  ** native
*)

(* Memory model: test the _publish idiom *)

module O = Opt.Make(struct let pgm = "forbidden"end)
module Config = O.Config
let verbose = Config.verbose

module OK = Outcome.OK
module NO = Outcome.NO

(* Types of shared memory locations *)

module IntRef = struct
  let null = ref (-1)
  type t = int ref
  let empty _ = ref 0
  let reinit r = r := 0
end

module IntRefRef = struct
  type t = int ref ref
  let empty _ = ref (IntRef.null)
  let reinit r = r := IntRef.null
end

module OutPointer = struct
  type t = bool
  let compare = Bool.compare
  let pp = function
    | true -> "null"
    | false -> "p"
end

(* Publication tests *)

module Publish = struct

(* Publication of int ref: the initial value of published
 *  pointer must be the one reader gets by derreferencing pointer:
 *    T0        |       T1
 *  ------------+-------------
 *  q := ref 1  | let p = !q in
 *              | let v = !p in
 *              | ..
 * If p is the fresh reference allocated by T0, then v must be 1.
 *)

  module IntRef = struct

    module O0 = OutPointer

    module O1 = Outcome.Int

    module Key =
      Outcome.Make(O0)(O1)
        (struct
          let name = "Publish+intref"
          let tag0 = "q"
          let tag1 = "r0"
          let ok p r0 = not p && r0 <> 1
        end)(NO)

    module Env = Shared.One(IntRefRef)

    (* publish *)
    type out0 = unit
    let code0 rr = rr := ref 1

    (* Dereference twice *)
    type out1 = bool * int
    let code1 rr =
      let r = !rr in
      let v = !r in
      r == IntRef.null,v

    let out2key _ () (p,v) = Key.make p v

  end

  (* Publication of string reference, with subsequent updates.
   * Notice that if publication is not properly protected,
   * the test may crash by attempting to print junk *)
  module String = struct

    let null = "*null*"
    module StringRef = struct
      type t =  string ref
      let empty _ = ref null
      let reinit r = r := null
    end

    let sz = 8
    let updates = List.init sz (Printf.sprintf "%d")
    module StringSet = Set.Make(String)


    let valid =
      let set = StringSet.of_list (null::updates) in
      fun s -> StringSet.mem s set

    module Key = struct

      type t = string

      let compare = String.compare

      let ok s = not (valid s)

      let allowed = false

      let name = "Publish+string"

      let pp chan s = Printf.fprintf chan "r0=%s" s

    end

    module Env = Shared.One(StringRef)

    (* publish *)
    type out0 = unit
    let code0 q =
      q := Printf.sprintf "%d" 0 ; (* publish *)
      List.init (sz-1) (fun k -> k+1)
      |>
        List.iter
          (fun v ->
            q := Printf.sprintf "%d" v)

    (* Dereference twice *)
    type out1 = string
    let code1 q = Domain.cpu_relax () ; !q

    let out2key _ () v = v

  end
end

(* One _allowed_ publication tests:
   array index is published, not a reference *)

module MPaddr = struct
  module Key =
    Outcome.Make(Outcome.Int)(Outcome.Int)
      (struct
        let name = "MP+addr"
        let tag0 = "idx"
        let tag1 = "v"
        let ok idx v = idx=1 && v=0
      end)(OK)

  module Env =
    Shared.Make
      (struct
        type t = int array
        let empty _ = Array.make 1 0
        let reinit t = t.(0) <- 0
      end)(IntRef)

  type out0 = unit
  let code0 (t,r) = (* publish int... *)
    t.(0) <- 1 ;
    r := 1

  type out1 = int * int
  let code1 (t,r) = (* Address dependency *)
    let idx = !r in
    let i = idx land 128 in
    let v = t.(i) in
    idx,v

  let out2key _ () (idx,v) = Key.make idx v
end

module Forbid(C:Opt.Config) = struct
  module Run = Run.Make(C)
  module T1 = Run(Publish.IntRef)
  let () = T1.zyva()
  module T2 = Run(Publish.String)
  let () = T2.zyva()
end

module Allow(C:Opt.Config) = struct
  module Run = Run.Make(C)
  module T1 = Run(MPaddr)
  let () = T1.zyva()
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
