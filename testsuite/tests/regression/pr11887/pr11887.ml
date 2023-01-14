(* TEST
   * native
     ocamlopt_flags = "-dcmm-invariants"
*)

module Constant = struct
  type ('a, 'b) result =
    | Ok of 'a
    | Error of 'b


  module S = struct
    type t =
      [
        `B0
      (* renaming the following variants makes the problem go away *)
      | `CA
      | `CH
      | `CI
      | `CL
      | `Other of string
      ]
  end

  type a1 = [ `CA ]
  type a2 = [ `CH ]
  type a3 = [ `CL ]

  let ok = Ok ()
  let error s =
    Error s

  let foo =
    let current =
      let module T = struct
        type a1 =
          [ `Common
          | `Uuuu
          | `Ssss
          | `Rrrr
          ]

        type a2 = [ `Common ]

        type a3 =
          [ `Common  ]
      end
      in
      fun ~s typ ->

        match s with
        | `None           -> error s
        | #a1         ->
          (match typ with
           | #T.a1 -> ok
           | _ -> error s)
        | #a2    ->
          (match typ with
           | #T.a2 -> ok
           | _ -> error s)
        | #a3 ->
          (match typ with
           | #T.a3 -> ok
           | _ -> error s)
        | #S.t -> error s
    in
    current
end

module Constr = struct
  type ('a, 'b) result =
    | Ok of 'a
    | Error of 'b


  module S = struct
    type t =
      [
        `B0 of int
      (* renaming the following variants makes the problem go away *)
      | `CA of int
      | `CH of int
      | `CI of int
      | `CL of int
      | `Other
      ]
  end

  type a1 = [ `CA of int ]
  type a2 = [ `CH of int ]
  type a3 = [ `CL of int ]

  let ok = Ok ()
  let error s =
    Error s

  let foo =
    let current =
      let module T = struct
        type a1 =
          [ `Common
          | `Uuuu
          | `Ssss
          | `Rrrr
          ]

        type a2 = [ `Common ]

        type a3 =
          [ `Common  ]
      end
      in
      fun ~s typ ->

        match s with
        | `None           -> error s
        | #a1         ->
          (match typ with
           | #T.a1 -> ok
           | _ -> error s)
        | #a2    ->
          (match typ with
           | #T.a2 -> ok
           | _ -> error s)
        | #a3 ->
          (match typ with
           | #T.a3 -> ok
           | _ -> error s)
        | #S.t -> error s
    in
    current
end
