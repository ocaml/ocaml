open Std_internal

type t = Location_list.t list

let create () = []

let insert t ~location_list =
(*
  let size_so_far =
    List.fold t
      ~init:0
      ~f:(fun size loc_list -> size + Location_list.size loc_list)
  in
  let attribute_referencing_the_new_list =
    Attribute_value.create_location
      ~offset_from_start_of_debug_loc:size_so_far
  in
*)
  let attribute_referencing_the_new_list =
    Attribute_value.create_location
      ~location_list_label:(Location_list.label location_list)
  in
  (location_list::t), attribute_referencing_the_new_list

let size t =
  List.fold t
    ~init:0
    ~f:(fun size loc_list -> size + Location_list.size loc_list)

let emit t ~emitter =
  List.iter (List.rev t) ~f:(Location_list.emit ~emitter)
