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


let check_noalloc name f =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  let _x = f () in
  let a2 = Gc.allocated_bytes () in
  let alloc = (a2 -. 2. *. a1 +. a0) in

  (* is there a better to test whether we run in native code? *)
  match Filename.basename Sys.argv.(0) with
  | "program.byte" | "program.byte.exe" -> ()
  | "program.native" | "program.native.exe" ->
      if alloc > 100. then
        failwith (Printf.sprintf "%s; alloc = %.0f" name alloc)
  | _ -> assert false

module GPR_109 = struct

  let f () =
    let r = ref 0. in
    for i = 1 to 1000 do
      let x = float i in
      let y = if i mod 2 = 0 then x else x +. 1. in
      r := !r +. y
    done;
    !r

  let () = check_noalloc "gpr 1O9" f
end


let unbox_classify_float () =
  let x = ref 100. in
  for i = 1 to 1000 do
    assert (classify_float !x = FP_normal);
    x := !x +. 1.
  done

let unbox_compare_float () =
  let module M = struct type sf = { mutable x: float; y: float; } end in
  let x = { M.x=100. ; y=1. } in
  for i = 1 to 1000 do
    assert (compare x.M.x x.M.y >= 0);
    x.M.x <- x.M.x +. 1.
  done


let unbox_float_refs () =
  let r = ref nan in
  for i = 1 to 1000 do r := !r +. float i done

let unbox_let_float () =
  let r = ref 0. in
  for i = 1 to 1000 do
    let y =
      if i mod 2 = 0 then nan else float i
    in
    r := !r +. (y *. 2.)
  done

type block =
  { mutable float : float;
    mutable int32 : int32 }

let make_some_block float int32 =
  { float; int32 }

let unbox_record_1 float int32 =
  (* There is some let lifting problem to handle that case with one
     round, this currently requires 2 rounds to be correctly
     recognized as a mutable variable pattern *)
  (* let block = (make_some_block [@inlined]) float int32 in *)
  let block = { float; int32 } in
  for i = 1 to 1000 do
    let y_float =
      if i mod 2 = 0 then nan else Pervasives.float i
    in
    block.float <- block.float +. (y_float *. 2.);
    let y_int32 =
      if i mod 2 = 0 then Int32.max_int else Int32.of_int i
    in
    block.int32 <- Int32.(add block.int32 (mul y_int32 2l))
  done
  [@@inline never]
  (* Prevent inlining to test that the type is effectively used *)

let unbox_record () =
  unbox_record_1 3.14 12l

let r = ref 0.

let unbox_only_if_useful () =
  for i = 1 to 1000 do
    let x =
      if i mod 2 = 0 then 1.
      else 0.
    in
    r := x; (* would force boxing if the let binding above were unboxed *)
    r := x  (* use [x] twice to avoid elimination of the let-binding *)
  done

let () =
  let flambda =
    match Sys.getenv "FLAMBDA" with
    | "true" -> true
    | "false" -> false
    | _ -> failwith "Cannot determine is flambda is enabled"
    | exception Not_found -> failwith "Cannot determine is flambda is enabled"
  in

  check_noalloc "classify float" unbox_classify_float;
  check_noalloc "compare float" unbox_compare_float;
  check_noalloc "float refs" unbox_float_refs;
  check_noalloc "unbox let float" unbox_let_float;
  check_noalloc "unbox only if useful" unbox_only_if_useful;

  if flambda then begin
    check_noalloc "float and int32 record" unbox_record;
  end;

  ()
