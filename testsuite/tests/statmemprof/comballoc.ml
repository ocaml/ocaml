(* TEST
 flags = "-g";
 {
   reference = "${test_source_directory}/comballoc.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/comballoc.opt.reference";
   native;
 }
*)

(* Tests that combined allocations are counted correctly by statmemprof *)

module MP = Gc.Memprof

(* A single 5-word allocation - header plus 4 content words *)

let f5 n = (n,n,n,n)

(* A combined 12-word allocation: 5 words, 4 words, and 3 words *)

let[@inline never] f12 n =
  (n, (n, n, f5 n))

let test sampling_rate =
  let allocs = Array.make 257 0 in   (* block size -> allocated samples *)
  let deallocs = Array.make 257 0 in (* block size -> minor-dealloc samples *)
  let promotes = Array.make 257 0 in (* block size -> promoted samples *)
  let callstacks = Array.make 257 None in (* block size -> callstack option *)
  let _:MP.t  = MP.start ~callstack_size:10  ~sampling_rate
    { MP.null_tracker with
      (* checks all allocations with a given block size have the same callstack *)
      alloc_minor = (fun info ->
        allocs.(info.size) <- allocs.(info.size) + info.n_samples;
        begin match callstacks.(info.size) with
        | None -> callstacks.(info.size) <- Some info.callstack
        | Some s -> assert (s = info.callstack)
        end;
        Some (info.size, info.n_samples));
      dealloc_minor = (fun (sz,n) ->
        deallocs.(sz) <- deallocs.(sz) + n);
      promote = (fun (sz,n) ->
        promotes.(sz) <- promotes.(sz) + n;
        None);
    } in
  let iter = 100_000 in
  let arr = Array.make iter (0,0,0,0) in
  for i = 0 to Array.length arr - 1 do
    (* extract the 5-word alloc from a 12-word comballoc *)
    let (_, (_, _, x)) = Sys.opaque_identity f12 i in
    arr.(i) <- x;
  done;
  Gc.minor ();
  MP.stop ();
  (* use arr, so it's still alive here and is not collected *)
  ignore (Sys.opaque_identity arr);
  for i = 0 to 256 do
    assert (deallocs.(i) + promotes.(i) = allocs.(i));
    if allocs.(i) > 0 then begin
      let total = (i + 1) * iter in
      (* allocs.(i) / total is
           Binomial(total, rate) / total
         which is approx.
           Normal(total * rate, total * rate*(1-rate)) / total
         which is
           Normal(1, rate*(1-rate) / total)
         which has stddev sqrt(rate*(1-rate)/total)
         which is less than 10^-3 for the values here.
         So, an error of 0.005 (enough to make %.2f print differently)
         is a 5-sigma event, with probability less than 3*10^-7 *)
      Printf.printf "%d: %.2f %b\n" i
        (float_of_int allocs.(i) /. float_of_int total)
        (promotes.(i) > 1000);
      (match callstacks.(i) with
       | Some s -> Printexc.print_raw_backtrace stdout s
       | None -> assert false)
    end
  done

let () =
  List.iter test [0.42; 0.01; 0.83]


let no_callback_after_stop trigger =
  let stopped = ref false in
  let cnt = ref 0 in
  let _:MP.t = MP.start ~callstack_size:0 ~sampling_rate:1.
    { MP.null_tracker with
      alloc_minor = (fun info ->
        assert(not !stopped);
        incr cnt;
        if !cnt > trigger then begin
          MP.stop ();
          stopped := true
        end;
        None);
    } in
  for i = 0 to 1000 do ignore (Sys.opaque_identity f12 i) done;
  assert !stopped

let () =
  for i = 0 to 1000 do no_callback_after_stop i done;
  Printf.printf "OK\n"
