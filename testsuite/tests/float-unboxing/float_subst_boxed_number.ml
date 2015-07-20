module PR_6686 = struct
  type t =
   | A of float
   | B of (int * int)

  let rec foo = function
   | A x -> x
   | B (x, y) -> float x +. float y

  let (_ : float) = foo (A 4.)
end

module PR_6770 = struct
  type t =
  | Constant of float
  | Exponent of (float * float)

  let to_string = function
    | Exponent (_b, _e) ->
      ignore _b;
      ignore _e;
      ""
    | Constant _ -> ""

  let _ = to_string (Constant 4.)
end


module GPR_109 = struct

  let f () =
    let r = ref 0. in
    for i = 1 to 1000 do
       r := !r +. float i
    done;
    !r

  let test () =
    let a0 = Gc.allocated_bytes () in
    let a1 = Gc.allocated_bytes () in
    let _x = f () in
    let a2 = Gc.allocated_bytes () in
    let alloc = (a2 -. 2. *. a1 +. a0) in
    assert(alloc < 100.)

    let () =
      (* is there a better to test whether we run in native code? *)
      match Filename.basename Sys.argv.(0) with
      | "program.byte" | "program.byte.exe" -> ()
      | "program.native" | "program.native.exe" -> test ()
      | _ -> assert false
end
