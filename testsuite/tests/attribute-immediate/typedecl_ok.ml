type t [@@immediate]

(* [@@immediate] tag here is unnecessary but valid since t has it *)
type s = t [@@immediate]

(* Again, valid alias even without tag *)
type r = s
