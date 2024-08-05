(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

let print_at_domain_exit ~domain ~thread i =
  Domain.at_exit (fun () ->
    Printf.printf "(%d) %s domain exit; %s thread\n%!"
      i domain thread
  )

let print_at_thread_exit ~domain ~thread i =
  Thread.at_exit (fun () ->
    Printf.printf "[%d] %s domain; %s thread exit\n%!"
      i domain thread
  )

let print_at_exit ~domain ~d ~thread ~t =
  print_at_thread_exit ~domain ~thread t;
  print_at_domain_exit ~domain ~thread d

(* In addition to printing all of the messages, the order in which the messages
   are printed is important. [at_exit] specifies that the registed
   functions are called in 'last in, first out' order: the functions most
   recently added with [at_exit] is called first. *)
let _ =
  let domain = "main" in
  let thread = "main" in
  print_at_exit
    (* those callbacks run after all others *)
    ~domain ~d:4 ~thread ~t:4;
  let d = Domain.spawn (fun () ->
    let domain = "child" in
    let t = Thread.create (fun () ->
      let thread = "child" in
      print_at_exit
        (* The thread will be joined right below, so
           this thread callback will run first.
           But another domain callback will be registered later
           on this domain, so this domain callback will run second. *)
        ~domain ~d:2 ~thread ~t:1;
    ) ()
    in
    Thread.join t;
    print_at_exit
      (* We already joined a thread so this callback will run second.
         The domain will be joined right below, so the present
         callback will run first. *)
      ~domain ~d:1 ~thread ~t:2;
  )
  in
  Domain.join d;
  let t = Thread.create (fun () ->
    let thread = "child" in
    print_at_exit ~domain ~d:3 ~thread ~t:3;
  ) ()
  in
  Thread.join t
