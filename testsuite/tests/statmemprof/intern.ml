(* TEST *)

module MP = Gc.Memprof

let alloc_tracker on_alloc =
  { MP.null_tracker with
    alloc_minor = (fun info -> on_alloc info; None);
    alloc_major = (fun info -> on_alloc info; None);
  }

(* avoiding constant folding, make a value tree consisting of n words *)

type t = O | IIi of int | IIt of t | IIIi of int * int | IIIt of t * t
let rec t_tree k n = match n with
   | len when len <= 1 -> O
   | 2 -> IIi k
   | 3 -> IIIi (k,k)
   | 4 -> IIt (IIi k)
   | 5 -> IIIt (IIi k, O)
   | 6 -> IIIt (IIIi (k,k), O)
   | len -> IIIt (t_tree k ((len-3)/2), t_tree k (len - 3 - (len-3)/2));;
let t_of_len n = t_tree 7 n;;

let marshalled_data = Hashtbl.create 17
let[@inline never] get_marshalled_data len : t =
  Marshal.from_string (Hashtbl.find marshalled_data len) 0
let precompute_marshalled_data lo hi =
  for len = lo to hi do
    if not (Hashtbl.mem marshalled_data len) then
      Hashtbl.add marshalled_data len (Marshal.to_string (t_of_len len) [])
  done

let root = ref []
let[@inline never] do_intern lo hi cnt keep =
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := get_marshalled_data i :: !root
    done;
    if not keep then root := []
  done

(* `get_marshalled_data i` should allocate `i` words with source
 * `Marshal`, in blocks of size 1 or 2. So `do_intern lo hi cnt _`
 * should allocate (hi+lo)(hi-lo+1)/2 words. *)

let check_nosample () =
  Printf.printf "check_nosample\n%!";
  precompute_marshalled_data 2 3000;
  let fail_on_alloc _ =
    Printf.printf "Callback called with sampling_rate = 0\n";
    assert(false)
  in
  let _:MP.t =
    MP.start ~callstack_size:10 ~sampling_rate:0.
                 (alloc_tracker fail_on_alloc)
  in
  do_intern 2 3000 1 false;
  MP.stop ()

let () = check_nosample ()

let check_counts_full_major force_promote =
  Printf.printf "check_counts_full_major\n%!";
  precompute_marshalled_data 2 3000;
  let nalloc_minor = ref 0 in
  let nalloc_major = ref 0 in
  let enable = ref true in
  let npromote = ref 0 in
  let ndealloc_minor = ref 0 in
  let ndealloc_major = ref 0 in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:0.01
    {
      alloc_minor = (fun _ ->
        if not !enable then None
        else Some (incr nalloc_minor)
      );
      alloc_major = (fun _ ->
        if not !enable then None
        else Some (incr nalloc_major)
      );
      promote = (fun _ ->
        Some (incr npromote)
      );
      dealloc_minor = (fun _ ->
        incr ndealloc_minor
      );
      dealloc_major = (fun _ ->
        incr ndealloc_major
      );
    }
  in
  do_intern 2 3000 1 true;
  enable := false;
  assert (!ndealloc_minor = 0 && !ndealloc_major = 0);
  if force_promote then begin
    Gc.full_major ();
    assert (!ndealloc_minor = 0 && !ndealloc_major = 0 &&
            !npromote = !nalloc_minor);
    root := [];
    Gc.full_major ();
    assert (!ndealloc_minor = 0 &&
            !ndealloc_major = !nalloc_minor + !nalloc_major);
  end else begin
    root := [];
    Gc.minor ();
    Gc.full_major ();
    Gc.full_major ();
    assert (!nalloc_minor = !ndealloc_minor + !npromote &&
            !ndealloc_major = !npromote + !nalloc_major)
  end;
  MP.stop ()

let () =
  check_counts_full_major false;
  check_counts_full_major true

let check_no_nested () =
  Printf.printf "check_no_nested\n%!";
  precompute_marshalled_data 2 300;
  let in_callback = ref false in
  let cb _ =
    assert (not !in_callback);
    in_callback := true;
    do_intern 100 200 1 false;
    in_callback := false;
    ()
  in
  let cb' _ = cb (); Some () in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
    {
      alloc_minor = cb';
      alloc_major = cb';
      promote = cb';
      dealloc_minor = cb;
      dealloc_major = cb;
    }
  in
  do_intern 100 200 1 false;
  MP.stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  precompute_marshalled_data lo hi;
  let smp = ref 0 in
  let alloc (info:MP.allocation) =
    (* We also allocate the list constructor in the minor heap,
       so we filter that out. *)
    if info.source = Marshal then begin
      assert (info.size = 1 || info.size = 2);
      assert (info.n_samples > 0);
      smp := !smp + info.n_samples
    end
  in
  let _:MP.t =
    MP.start ~callstack_size:10 ~sampling_rate:rate
      (alloc_tracker alloc)
  in
  do_intern lo hi cnt false;
  MP.stop ();

  (* The probability distribution of the number of samples follows a
     binomial distribution of parameters tot_alloc and rate. Given
     that tot_alloc*rate and tot_alloc*(1-rate) are large (i.e., >
     100), this distribution is approximately equal to a normal
     distribution. We compute a 1e-8 confidence interval for !smp
     using quantiles of the normal distribution, and check that we are
     in this confidence interval. *)
  let tot_alloc = cnt*(lo+hi)*(hi-lo+1)/2 in
  assert (float tot_alloc *. rate > 100. &&
          float tot_alloc *. (1. -. rate) > 100.);
  let mean = float tot_alloc *. rate in
  let stddev = sqrt (float tot_alloc *. rate *. (1. -. rate)) in
  (* This should fail approximately one time in 100,000,000 *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 2 3000 3 0.00001;
  check_distrib 2 3000 1 0.0001;
  check_distrib 2 2000 1 0.01;
  check_distrib 2 2000 1 0.9;
  check_distrib 300000 300000 20 0.1

let () =
  Printf.printf "OK !\n"
