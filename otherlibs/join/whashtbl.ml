type ('a,'b) t =
    { 
    size: int;
    table: ('a,int) Hashtbl.t;
    mutable array: 'b Weakarray.t ref
    } 

let create n =
  { 
  size = n;
  table = Hashtbl.create n;
  array = ref ( Weakarray.create n)
  } 

let add t a b =
  let wk = Weakarray.ref_add t.array b in
  Hashtbl.add t.table a wk
    
let find t a = Weakarray.get (!(t.array)) (Hashtbl.find t.table a)
    
let remove t a = 
  try
    let wk = Hashtbl.find t.table a in
    Weakarray.free !(t.array) wk;
    Hashtbl.remove t.table a
  with Not_found -> ()

let h_to_list t =
  let l = ref [] in
  Hashtbl.iter (fun a b -> l:=(a,b):: !l) t;
  !l

let h_from_list n l =
  let t = Hashtbl.create n in
  List.iter (fun (a,b) -> Hashtbl.add t a b) l;
  t

let iter f t =
  let l = h_to_list t.table in
  List.iter (fun (a,b) -> f a (Weakarray.get !(t.array) b)) l

let to_list t =
  List.map (fun (a,b) -> (a,Weakarray.get !(t.array) b)) (h_to_list t.table)

let from_list n l =
  let t = create n in
  List.iter (fun (a,b) -> if b <> None then add t a b) l;
  t

let clear t =
  Hashtbl.clear t.table;
  t.array <- ref ( Weakarray.create t.size)

let clean t =
  let l = ref [] in
  Hashtbl.iter (fun a b -> 
    match Weakarray.get !(t.array) b with
      None -> 
        l := a :: !l;
        Weakarray.free !(t.array) b;
    | Some _ -> ()
    ) t.table;
  List.iter (fun i -> Hashtbl.remove t.table i) !l

(* ici, on accede directement a la table de pointeurs faibles *)
let find_direct t i = Weakarray.get (!(t.array)) i
let add_direct t a b =
  let wk = Weakarray.ref_add t.array b in
  Hashtbl.add t.table a wk;
  wk
let get_direct t a = Hashtbl.find t.table a
