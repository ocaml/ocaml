open Camlp4Profiler;

value profile = load stdin;

value profile = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) profile;

List.iter
  (fun (k, v) -> Format.printf "%-75s: %d@." k v)
  profile;
