(* TEST
   * hassysthreads
   include systhreads
   ** native
   ** bytecode
*)

let status = ref 0

let () =
  let t =
    Thread.create (fun () ->
        try
        assert (!status = 0);
        status := 1;
        Thread.exit () [@ocaml.warning "-3"]
      with exn ->
        assert (!status = 1);
        status := 2;
        raise exn
      ) ()
  in
  Thread.join t;
  assert (!status = 2);
  Printf.printf "OK\n"
