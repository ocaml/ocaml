(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Obj

(**** Object representation ****)

let object_tag = 248

let last_id = ref 0
let new_id () =
  let id = !last_id in incr last_id; id

(**** Object copy ****)

let copy (o : < .. >) : 'a =
  let o = Obj.repr (o : 'a) in
  let s = Obj.size o in
  let r = Obj.new_block object_tag s in
  Obj.set_field r 0 (Obj.field o 0);
  Obj.set_field r 1 (Obj.repr !last_id); incr last_id;
  for i = 2 to s - 1 do Obj.set_field r i (Obj.field o i) done;
  Obj.obj r

(**** Compression options ****)
(* Parameters *)
type params = {
    mutable compact_table : bool;
    mutable copy_parent : bool;
    mutable clean_when_copying : bool;
    mutable retry_count : int;
    mutable bucket_small_size : int
  } 

let params = {
  compact_table = true;
  copy_parent = true;
  clean_when_copying = true;
  retry_count = 3;
  bucket_small_size = 16
} 

(**** Parameters ****)

let step = Sys.word_size / 16
let first_bucket = 0
let bucket_size = 32                    (* Must be 256 or less *)
let initial_object_size = 2

(**** Index ****)

type label = int

let label_count = ref 0

let next label =
  incr label_count;
  let label = label + step in
  if label mod (step * bucket_size) = 0 then
    label + step * (65536 - bucket_size)
  else
    label

let decode label =
  (label / 65536 / step, (label mod (step * bucket_size)) / step)

(**** Items ****)

type item

let dummy_item = (magic () : item)

(**** Buckets ****)

type bucket = item array

let version = ref 0

let set_bucket_version (bucket : bucket) =
  bucket.(bucket_size) <- (magic !version : item)

let bucket_version bucket =
  (magic bucket.(bucket_size) : int)

let bucket_list = ref []

let empty_bucket = [| |]

let new_bucket () =
  let bucket = Array.create (bucket_size + 1) dummy_item in
  set_bucket_version bucket;
  bucket_list := bucket :: !bucket_list;
  bucket

let copy_bucket bucket =
  let bucket = Array.copy bucket in
  set_bucket_version bucket;
  bucket.(bucket_size) <- (magic !version : item);
  bucket_list := bucket :: !bucket_list;
  bucket

(**** Make a clean bucket ****)

let new_filled_bucket pos methods =
  let bucket = new_bucket () in
  List.iter
    (fun (lab, met) ->
       let (buck, elem) = decode lab in
       if buck = pos then
         bucket.(elem) <- (magic met : item))
    (List.rev methods);
  bucket

(**** Bucket merging ****)

let small_buckets = ref (Array.create 10 [| |])
let small_bucket_count = ref 0

let insert_bucket bucket =
  let length = Array.length !small_buckets in
  if !small_bucket_count >= length then begin
    let new_array = Array.create (2 * length) [| |] in
    Array.blit !small_buckets 0 new_array 0 length;
    small_buckets := new_array
  end;
  !small_buckets.(!small_bucket_count) <- bucket;
  incr small_bucket_count

let remove_bucket n =
  !small_buckets.(n) <- !small_buckets.(!small_bucket_count - 1);
  decr small_bucket_count

let bucket_used b =
  let n = ref 0 in
  for i = 0 to bucket_size - 1 do
    if b.(i) != dummy_item then incr n
  done;
  !n

let small_bucket b = bucket_used b <= params.bucket_small_size

exception Failed

let rec except e =
  function
    [] -> []
  | e'::l -> if e == e' then l else e'::(except e l)

let merge_buckets b1 b2 =
  for i = 0 to bucket_size - 1 do
    if
      (b2.(i) != dummy_item) & (b1.(i) != dummy_item) & (b2.(i) != b1.(i))
    then
      raise Failed
  done;
  for i = 0 to bucket_size - 1 do
    if b2.(i) != dummy_item then
      b1.(i) <- b2.(i)
  done;
  bucket_list := except b2 !bucket_list;
  b1

let rec choose bucket i =
  if (i > 0) & (!small_bucket_count > 0) then begin
    let n = Random.int !small_bucket_count in
    if not (small_bucket !small_buckets.(n)) then begin
      remove_bucket n; choose bucket i
    end else
      try
        merge_buckets !small_buckets.(n) bucket
      with Failed ->
        choose bucket (i - 1)
  end else begin
    insert_bucket bucket;
    bucket
  end

let compact b =
  if
    (b != empty_bucket) & (bucket_version b = !version) & (small_bucket b)
  then
    choose b params.retry_count
  else
    b

let compact_buckets buckets =
  for i = first_bucket to Array.length buckets - 1 do
    buckets.(i) <- compact buckets.(i)
  done

(**** Labels ****)

let first_label = first_bucket * 65536 * step

let last_label = ref first_label
let methods = Hashtbl.create 101

let new_label () =
  let label = !last_label in
  last_label := next !last_label;
  label

let new_method met =
  try
    Hashtbl.find methods met
  with Not_found ->
    let label = new_label () in
    Hashtbl.add methods met label;
    label

let new_anonymous_method =
  new_label

(**** Types ****)

type object = t array

(**** Sparse array ****)

module Vars = Map.Make(struct type t = string let compare = compare end)
type vars = int Vars.t

module Meths = Map.Make(struct type t = string let compare = compare end)
type meths = label list Meths.t

type obj_init
type table =
 { mutable buckets: bucket array;
   mutable methods: meths;
   mutable saved_methods: meths list;
   mutable meth_defs: (label * item) list;
   mutable vars: vars;
   mutable saved_vars: vars list;
   mutable saved_var_lst: int option list;
   mutable size: int;
   mutable init: obj_init list list }

let table_count = ref 0

let new_table () =
  incr table_count;
  { buckets = [| |];
    methods = Meths.empty;
    saved_methods = [];
    meth_defs = [];
    vars = Vars.empty;
    saved_vars = [];
    saved_var_lst = [];
    size = initial_object_size;
    init = [[]; []]}

let copy_table array1 array2 =
  incr version;
  array1.buckets <- Array.copy array2.buckets;
  array1.methods <- array2.methods;
  array1.saved_methods <- [];
  array1.meth_defs <- array2.meth_defs;
  array1.vars <- array2.vars;
  array1.saved_vars <- [];
  array1.saved_var_lst <- array1.saved_var_lst;
  array1.size <- array2.size;
  array1.init <- array2.init

let resize array new_size =
  let old_size = Array.length array.buckets in
  if new_size > old_size then begin
    let new_buck = Array.create new_size empty_bucket in
    Array.blit array.buckets 0 new_buck 0 old_size;
    array.buckets <- new_buck
 end

let put array label element =
  let (buck, elem) = decode label in
  resize array (buck + 1);
  let bucket = ref (array.buckets.(buck)) in
  if !bucket == empty_bucket then begin
    bucket := new_bucket ();
    array.buckets.(buck) <- !bucket
  end;
  if !bucket.(elem) != element then begin
    if
      (bucket_version !bucket < !version)
          &
      (!bucket.(elem) != dummy_item)
    then begin
      if params.clean_when_copying then
        bucket := new_filled_bucket buck array.meth_defs
      else
        bucket := copy_bucket !bucket;
      array.buckets.(buck) <- !bucket
    end;
    !bucket.(elem) <- element
  end

(**** Classes ****)

type t
type class_info =
  {mutable obj_init: t;
   mutable class_init: table -> unit;
   mutable table: table}

let set_initializer table init =
  match table.init with
    l::l'::l'' ->
      let i =
        List.fold_right
          (fun init2 init1 -> (magic init1 : obj_init -> obj_init) init2)
          l init
      in
        table.init <- (i::l')::l''
  | _ ->
      invalid_arg "Fatal error in Oo.set_initializer."

let inheritance table cl vars meths =
  if
    params.copy_parent
    && (table.methods = Meths.empty) && (table.size = initial_object_size)
    && (table.init = [[]; []])
  then begin
    copy_table table cl.table;
    table.init <- table.init@[[]];
    table.vars <- 
      List.fold_left
        (fun s v ->
           try Vars.add v (Vars.find v table.vars) s with Not_found -> s)
        Vars.empty
        vars;
    table.methods <-
      List.fold_left
        (fun s m ->
           try Meths.add m (Meths.find m table.methods) s with Not_found -> s)
        Meths.empty
        meths
  end else begin
    table.init <- []::table.init;
    table.saved_vars <- table.vars::table.saved_vars;
    table.vars <-
      List.fold_left
        (fun s v ->
           try Vars.add v (Vars.find v table.vars) s with Not_found -> s)
        Vars.empty
        vars;
    table.saved_methods <- table.methods::table.saved_methods;
    table.methods <-
      List.fold_left
        (fun s m ->
           try Meths.add m (Meths.find m table.methods) s with Not_found -> s)
        Meths.empty
        meths;
    cl.class_init table;
    table.vars <-
      List.fold_left
        (fun s v ->
           try Vars.add v (Vars.find v table.vars) s with Not_found -> s)
        (List.hd table.saved_vars)
        vars;
    table.saved_vars <- List.tl table.saved_vars;
    table.methods <-
      List.fold_left
        (fun s m ->
           try Meths.add m (Meths.find m table.methods) s with Not_found -> s)
        (List.hd table.saved_methods)
        meths;
    table.saved_methods <- List.tl table.saved_methods
  end

let get_method_labels table name =
  try
    Meths.find name table.methods
  with Not_found ->
    let label = new_anonymous_method () in
    table.methods <- Meths.add name [label] table.methods;
    [label]

let get_method_label table name =
  List.hd (get_method_labels table name)

let set_method table name element =
  List.iter
    (function label ->
       table.meth_defs <- (label, element) :: table.meth_defs;
       put table label element)
    (get_method_labels table name)

let get_method table label =
  let (buck, elem) = decode label in
  table.buckets.(buck).(elem)

let new_slot table =
  let index = table.size in
  table.size <- index + 1;
  index

let get_variable table name =
  try
    Vars.find name table.vars
  with Not_found ->
    let index = new_slot table in
    table.vars <- Vars.add name index table.vars;
    index

let hide_variable table name =
  try
    let i = Vars.find name table.vars in
    table.vars <- Vars.remove name table.vars;
    table.saved_var_lst <- Some i::table.saved_var_lst
  with Not_found ->
    table.saved_var_lst <- None::table.saved_var_lst

let rec list_remove name =
  function
    [] ->
       invalid_arg "Fatal error in Oo.get_private_variable"
  | (n, _) as a::l ->
       if name = n then l
       else a::list_remove name l

let get_private_variable table name =
  let index =
    try Vars.find name table.vars with Not_found -> new_slot table
  in
  table.vars <-
    begin match List.hd table.saved_var_lst with
      None ->
        Vars.remove name table.vars
    | Some i ->
        Vars.add name i table.vars
    end;
  table.saved_var_lst <- List.tl table.saved_var_lst;
  index

let method_count = ref 0
let inst_var_count = ref 0

let new_object table =
  let obj = Array.create table.size (magic () : t) in
  obj.(0) <- (magic table.buckets : t);
  obj

let create_class class_info public_methods creator class_init =
  let table = new_table () in
  List.iter
    (function met ->
       table.methods <- Meths.add met [new_method met] table.methods)
    public_methods;
  class_init table;
  method_count := !method_count + List.length table.meth_defs;
  if params.compact_table then
    compact_buckets table.buckets;
  inst_var_count := !inst_var_count + table.size - 1;
  class_info.class_init <- class_init;
  class_info.table <- table;
  class_info.obj_init <- creator table

(**** Objects ****)

let create_object table =
  let obj = Obj.new_block object_tag table.size in
  Obj.set_field obj 0 (Obj.repr table.buckets);
  Obj.set_field obj 1 (Obj.repr (new_id ()));
  let initialization = Obj.magic List.hd (List.hd table.init) in
  initialization (Obj.obj obj)

let send obj lab =
  let (buck, elem) = decode lab in
  (magic obj : (object -> t) array array array).(0).(buck).(elem) obj

(**** Statistics ****)

type stats =
  { classes: int; labels: int; methods: int; inst_vars: int; buckets: int;
    distrib : int array; small_bucket_count: int; small_bucket_max: int }

let distrib () =
  let d = Array.create 32 0 in
  List.iter
    (function b ->
       let n = bucket_used b in
       d.(n - 1) <- d.(n - 1) + 1)
    !bucket_list;
  d

let stats () =
  { classes = !table_count; labels = !label_count;
    methods = !method_count; inst_vars = !inst_var_count;
    buckets = List.length !bucket_list; distrib = distrib ();
    small_bucket_count = !small_bucket_count;
    small_bucket_max = Array.length !small_buckets  }

let sort_buck lst =
  List.map snd
    (Sort.list (fun (n, _) (n', _) -> n <= n')
       (List.map (function b -> (bucket_used b, b)) lst))

let show_buckets () =
  List.iter
    (function b ->
       for i = 0 to bucket_size - 1 do
         print_char (if b.(i) == dummy_item then '.' else '*')
       done;
       print_newline ())
    (sort_buck !bucket_list)
