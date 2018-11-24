(* TEST
*)

let pass () = exit 0

let fail () = exit 1

(* raise Not_found from a finaliser, and check that it does not become
   a Not_found exception raised out of thin air. *)
let test_finaliser_raised () =
  (* allocate on the heap *)
  let x = Sys.opaque_identity [1.; 1.] |> List.rev |> ref in
  let finalised = ref false in
  let finalise () =
    finalised := true ;
    raise Not_found
  in
  Gc.finalise_last finalise !x ;
  let _ =
    try
      let hd0 = List.hd !x in
      (* dummy allocating action freeing the finalised block *)
      let f n = x := List.rev !x ; n = hd0 in
      let my_business () = Sys.opaque_identity (List.find f !x) in
      while not !finalised do
        ignore (my_business ())
      done ;
      fail () (* the test was optimised away *)
    with
    | Not_found -> fail ()
    | _ -> pass ()
  in
  !x

let _ = test_finaliser_raised ()
