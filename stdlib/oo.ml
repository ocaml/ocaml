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

let set_id o id =
  let id0 = !id in
  Array.unsafe_set (Obj.magic o : int array) 1 id0;
  id := id0 + 1

(**** Object copy ****)

(*
let copy o =
  let o = (Obj.obj (Obj.dup (Obj.repr o))) in
  let id = (Obj.magic last_id : int ref) in
  let id0 = !id in
  Array.unsafe_set (Obj.magic o : int array) 1 id0;
  id := id0 + 1;
  o
*)
let copy o =
  let o = (Obj.obj (Obj.dup (Obj.repr o))) in
  set_id o last_id;
  o

let f x = (x, x, x, x)

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

type obj = t array

(**** Sparse array ****)

module Vars = Map.Make(struct type t = string let compare = compare end)
type vars = int Vars.t

module Meths = Map.Make(struct type t = string let compare = compare end)
type meths = label Meths.t
module Labs = Map.Make(struct type t = label let compare = compare end)
type labs = bool Labs.t

type obj_init
(* The compiler assumes that the first field of this structure is [size]. *)
type table =
 { mutable size: int;
   mutable buckets: bucket array;
   mutable methods_by_name: meths;
   mutable methods_by_label: labs;
   mutable previous_states:
     (meths * labs * (label * item) list * vars *
      label list * string list) list;
   mutable hidden_meths: (label * item) list;
   mutable vars: vars;
   mutable initializers: (obj -> unit) list }

let table_count = ref 0

let new_table () =
  incr table_count;
  { buckets = [| |];
    methods_by_name = Meths.empty;
    methods_by_label = Labs.empty;
    previous_states = [];
    hidden_meths = [];
    vars = Vars.empty;
    initializers = [];
    size = initial_object_size }

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
  !bucket.(elem) <- element

(**** Classes ****)

let method_count = ref 0
let inst_var_count = ref 0

type t
type meth = item
type class_info =
  {mutable obj_init: t;
   mutable class_init: table -> bool -> obj_init;
   mutable table: table}

let get_method_label table name =
  try
    Meths.find name table.methods_by_name
  with Not_found ->
    let label = new_anonymous_method () in
    table.methods_by_name <- Meths.add name label table.methods_by_name;
    table.methods_by_label <- Labs.add label true table.methods_by_label;
    label

let set_method table label element =
  incr method_count;
  if Labs.find label table.methods_by_label then
    put table label element
  else
    table.hidden_meths <- (label, element) :: table.hidden_meths

let get_method table label =
  try List.assoc label table.hidden_meths with Not_found ->
  let (buck, elem) = decode label in
  table.buckets.(buck).(elem)

let narrow table vars virt_meths concr_meths =
  let virt_meth_labs = List.map (get_method_label table) virt_meths in
  table.previous_states <-
     (table.methods_by_name, table.methods_by_label, table.hidden_meths,
      table.vars, virt_meth_labs, vars)
     :: table.previous_states;
  table.vars <- Vars.empty;
  let by_name = ref Meths.empty in
  let by_label = ref Labs.empty in
  List.iter
    (function met ->
       let label = get_method_label table met in
       by_name := Meths.add met label !by_name;
       by_label :=
          Labs.add label
            (try Labs.find label table.methods_by_label with Not_found -> true)
            !by_label)
    concr_meths;
  List.iter2
    (fun met label ->
       by_name := Meths.add met label !by_name;
       by_label := Labs.add label false !by_label)
    virt_meths virt_meth_labs;
  table.methods_by_name <- !by_name;
  table.methods_by_label <- !by_label;
  table.hidden_meths <-
     List.fold_right
       (fun ((lab, _) as met) hm ->
          if List.mem lab virt_meth_labs then hm else met::hm)
       table.hidden_meths
       []

let widen table =
  let (by_name, by_label, saved_hidden_meths, saved_vars, virt_meths, vars) =
    List.hd table.previous_states
  in
  table.previous_states <- List.tl table.previous_states;
  table.vars <-
     List.fold_left
       (fun s v -> Vars.add v (Vars.find v table.vars) s)
       saved_vars vars;
  table.methods_by_name <- by_name;
  table.methods_by_label <- by_label;
  table.hidden_meths <-
     List.fold_right
       (fun ((lab, _) as met) hm ->
          if List.mem lab virt_meths then hm else met::hm)
       table.hidden_meths
       saved_hidden_meths

let get_class table cl = cl.class_init table false

let new_slot table =
  let index = table.size in
  table.size <- index + 1;
  index

let new_variable table name =
  let index = new_slot table in
  table.vars <- Vars.add name index table.vars;
  index

let get_variable table name =
  Vars.find name table.vars

let copy_variables class_info table =
  ();
  function () ->
  let template = class_info.obj_init in
  let max = class_info.table.size - 1 in
  let max' = table.size - 1 in
  let offset = max' - max in
  function obj ->
    for i = initial_object_size to max do
      (* XXX Hack *)
      Array.unsafe_set (Obj.magic obj : string array) (i + offset)
        (Array.unsafe_get (Obj.magic template : string array) i)
    done

let add_initializer table f =
  table.initializers <- f::table.initializers

let create_class class_info public_methods class_init creator =
  let table = new_table () in
  List.iter
    (function met ->
       let lab = new_method met in
       table.methods_by_name  <- Meths.add met lab table.methods_by_name;
       table.methods_by_label <-  Labs.add lab true table.methods_by_label)
    public_methods;
  let obj_init = class_init table true in
  inst_var_count := !inst_var_count + table.size - 1;
  if params.compact_table then
    compact_buckets table.buckets;
  table.initializers <- List.rev table.initializers;
  class_info.class_init <- class_init;
  class_info.table <- table;
  class_info.obj_init <- creator table obj_init

let create_table public_methods =
  let table = new_table () in
  List.iter
    (function met ->
       let lab = new_method met in
       table.methods_by_name  <- Meths.add met lab table.methods_by_name;
       table.methods_by_label <- Labs.add lab true table.methods_by_label)
    public_methods;
  table

let init_class table =
  inst_var_count := !inst_var_count + table.size - 1;
  if params.compact_table then
    compact_buckets table.buckets;
  table.initializers <- List.rev table.initializers

(**** Objects ****)

let create_object table =
  (* XXX Appel de [obj_block] *)
  let obj = Obj.new_block object_tag table.size in
  (* XXX Appel de [modify] *)
  Obj.set_field obj 0 (Obj.repr table.buckets);
  set_id obj last_id;
  (Obj.obj obj)

let rec iter_f obj =
  function
    []   -> ()
  | f::l -> f obj; iter_f obj l

let run_initializers obj table =
  let inits = table.initializers in
  if inits <> [] then
    iter_f obj inits

let object_from_struct cl_inf =
  (* XXX Appel de [obj_dup] *)
  let obj = (Obj.obj (Obj.dup (Obj.repr cl_inf.obj_init))) in
  set_id obj last_id;
  run_initializers (Obj.magic obj) cl_inf.table;
  obj

let send obj lab =
  let (buck, elem) = decode lab in
  (magic obj : (obj -> t) array array array).(0).(buck).(elem) obj

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
