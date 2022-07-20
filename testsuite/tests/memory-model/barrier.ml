(**********************************************)
(* Simple, work once, synchronisation barrier *)
(**********************************************)

type t = int Atomic.t array

let make sz = Array.init sz (fun _ -> Atomic.make 0)

let reinit t =
  for k=0 to Array.length t-1 do
    Atomic.set t.(k) 0
  done

let wait t id i =
  if i mod 2 = id then
    Atomic.set t.(i) 1
  else
    while Atomic.get t.(i) != 1 do
      Domain.cpu_relax ()
    done
