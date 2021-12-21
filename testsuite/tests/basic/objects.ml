(* TEST *)


(* Non-regression for bug #10763, fixed in #10764 *)

module W = struct
  let r = ref (object method m x = Printf.printf "BAD %i\n%!" x end)
end

let proxy = object method m = (!W.r) # m end

let () =
  W.r := object method m x = Printf.printf "OK %i\n%!" x end;
  proxy # m 3
