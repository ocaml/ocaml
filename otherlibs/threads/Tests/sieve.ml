open Printf
open Thread

let rec integers n ch =
  Event.sync (Event.send ch n);
  integers (n+1) ch

let rec sieve n chin chout =
  let m = Event.sync (Event.receive chin)
  in if m mod n = 0
     then sieve n chin chout
     else Event.sync (Event.send chout m);
          sieve n chin chout

let rec print_primes ch max =
  let n = Event.sync (Event.receive ch)
  in if n > max
     then ()
     else begin
            printf "%d\n" n; flush stdout;
            let ch_after_n = Event.new_channel ()
            in Thread.create (sieve n ch) ch_after_n;
               print_primes ch_after_n max
          end

let go max =
  let ch = Event.new_channel ()
  in Thread.create (integers 2) ch;
     print_primes ch max;;

let _ = go 1000

;;
