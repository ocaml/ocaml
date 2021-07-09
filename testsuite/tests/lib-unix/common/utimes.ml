(* TEST
* hasunix
include unix
readonly_files = "utimes.txt"
** bytecode
** native
*)

(* We do not check setting the "last access time" because it is hard to do so on
   some file systems. FAT, for example, only has a 1d resolution for this
   timestamp, and even NTFS can potentially delay the update of this timestamp
   by up to an hour.
*)

let txt = "utimes.txt"

(* To account for filesystems with large timestamp resolution (e.g. FAT - 2
   seconds for mtime)
*)
let close s t =
  abs_float (s -. t) < 10.

let check tm =
  let tm' = (Unix.stat txt).Unix.st_mtime in
  Printf.printf "tm ~ tm' (%B)\n" (close tm tm')

let () =
  let oc = open_out_bin txt in
  close_out oc;
  let tm = 1508391026.124 in
  Unix.utimes txt tm tm;
  check tm;
  let tn = Unix.time () in
  Unix.utimes txt 0. 0.;
  check tn
