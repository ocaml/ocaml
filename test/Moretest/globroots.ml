type t

external register: string -> t = "gb_register"
external get: t -> string = "gb_get"
external remove: t -> unit = "gb_remove"

let size = 1024

let _ =
  let a = Array.init size (fun i -> register (string_of_int i)) in
  while true do
    (* Check data *)
    for i = 0 to size - 1 do
      if get a.(i) <> string_of_int i then begin
        print_string "Error on "; print_int i; print_string ": ";
        print_string (String.escaped (get a.(i))); print_newline()
      end
    done;
    (* Change it randomly *)
    let i = Random.int size in
    remove a.(i);
    a.(i) <- register (string_of_int i);
    Gc.full_major();
    print_string "."; flush stdout
  done
