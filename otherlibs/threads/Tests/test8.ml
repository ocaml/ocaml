open Event

type 'a buffer_channel = { input: 'a channel; output: 'a channel }

let new_buffer_channel() =
  let ic = new_channel() in
  let oc = new_channel() in
  let buff = Queue.create() in
  let rec buffer_process front rear =
    match (front, rear) with
      ([], []) -> buffer_process [sync(receive ic)] []
    | (hd::tl, _) ->
        select [
          wrap (receive ic) (fun x -> buffer_process front (x::rear));
          wrap (send oc hd) (fun () -> buffer_process tl rear)
        ]
    | ([], _) -> buffer_process (List.rev rear) [] in
  Thread.create (buffer_process []) [];
  { input = ic; output = oc }

let buffer_send bc data =
  sync(send bc.input data)

let buffer_receive bc =
  receive bc.output

(* Test *)

let box = new_buffer_channel()
let ch = new_channel()

let f () =
  buffer_send box "un";
  buffer_send box "deux";
  sync (send ch 3)

let g () =
  print_int (sync(receive ch)); print_newline();
  print_string (sync(buffer_receive box)); print_newline();
  print_string (sync(buffer_receive box)); print_newline()

let _ =
  Thread.create f ();
  g()


