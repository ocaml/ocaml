(* $Id$ *)

class ['a,'b] c fun:(f : 'a -> 'b) = object
  val hash = Hashtbl.create 7
  method get key =
    try Hashtbl.find hash :key
    with Not_found ->
      let data = f key in
      Hashtbl.add hash :key :data;
      data
  method clear = Hashtbl.clear hash
  method reget key =
    Hashtbl.remove :key hash;
    let data = f key in
    Hashtbl.add hash :key :data;
    data
end
