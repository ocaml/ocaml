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

module Gadts = struct
  type keep = private Keep
  type drop = private Drop

  type _ t =
  | C00 : keep t
  | C01 : drop t
  | C02 : drop t
  | C03 : drop t
  | C04 : drop t
  | C05 : drop t
  | C06 : drop t
  | C07 : drop t
  | C08 : drop t
  | C09 : drop t
  | C10 : drop t
  | C11 : drop t
  | C12 : drop t
  | C13 : drop t
  | C14 : drop t
  | C15 : drop t
  | C16 : drop t
  | C17 : drop t
  | C18 : drop t
  | C19 : drop t
  | C20 : keep t
  | C21 : drop t
  | C22 : drop t
  | C23 : drop t
  | C24 : keep t
  | C25 : keep t
  | C26 : drop t
  | C27 : drop t
  | C28 : keep t

  let not_at_toplevel b =
    let f (x: keep t) =
      match x with
      | C00 -> 0
      | C20 ->
        begin
          (* The implementation of the local function optimisation will not
             handle well multiple bindings of the same function, so this
             code will reliably fail to compile with Closure if it ends up
             duplicated. *)
          let[@local] handler x = x + 2 in
          if b then handler 0 else handler 1
        end
      | C24 -> 2
      | C25 -> 3
      | C28 -> 4
    in
    f
end
