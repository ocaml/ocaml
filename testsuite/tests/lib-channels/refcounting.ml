(* TEST
   * expect
*)

(* Test the behavior of channel refcounting. *)

(* out_channels_list is the only function that increases the number of reference
  to a channel in the standard library *)
external out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"

let duplicate_and_close () =
  let l = out_channels_list () in
  List.iter Stdlib.close_out l

let rec loop n () =
  if n <> 0 then
    begin
      duplicate_and_close ();
      loop (n-1) ()
    end


let dls = List.map Domain.spawn (List.init 4 (fun _ -> loop 100))
let () = List.iter Domain.join dls

[%%expect{|
external out_channels_list : unit -> out_channel list
  = "caml_ml_out_channels_list"
val duplicate_and_close : unit -> unit = <fun>
val loop : int -> unit -> unit = <fun>
val dls : unit Domain.t list = [<abstr>; <abstr>; <abstr>; <abstr>]
|}]
