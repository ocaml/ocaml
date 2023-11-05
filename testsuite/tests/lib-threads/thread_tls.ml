(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)


module TLS = Thread.TLS

let k1 : int TLS.key = TLS.new_key (fun () -> 0)

let () =
  let n = 1_000_000 in
  let values = Array.make 20 0 in

  let run_thread ~d_idx ~t_idx () =
    for _i = 1 to n do
      let r = TLS.get k1 in
      TLS.set k1 (r + 1)
    done;
    values.(d_idx * 10 + t_idx) <- TLS.get k1
  in


  let threads : Thread.t list =
    List.concat @@
    List.init 2 @@ fun d_idx ->
    Domain.join @@
    Domain.spawn @@ fun () ->
    Array.to_list @@ 
    Array.init 10 (fun t_idx -> Thread.create (run_thread ~d_idx ~t_idx) ())
  in

  List.iter Thread.join threads;
  Array.iter (fun x -> assert (x = n)) values

let k2 : int ref TLS.key = TLS.new_key (fun () -> ref 0)
let k3 : int ref TLS.key = TLS.new_key (fun () -> ref 0)

let () =
  print_endline "starting";
  let res = ref (0, 0) in
  let run () =
    for _i = 1 to 1000 do
      let r2 = TLS.get k2 in
      incr r2;
      let r3 = TLS.get k3 in
      r3 := !r3 + 2
    done;
    res := !(TLS.get k2), !(TLS.get k3)
  in

  let t = Thread.create run () in
  Thread.join t;

  assert (!res = (1000, 2000));
  print_endline "ok"
