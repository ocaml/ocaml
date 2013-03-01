open Datarepr

  (*
type block_name =
  Block_anonymous of string * int
| Block_alloc of Location.t
| Block_typedef of Ident.t
| Block_name of Ident.t
| Block_path of path
    *)

type block_repr = {
    repr_tag : int option;
    repr_size : int option;
    repr_content : type_repr list option;
    repr_labels : string list option;
  }

and type_repr =
| Repr_variable of int
| Repr_unknown
| Repr_integer
| Repr_block of block_repr
| Repr_choice of (string * type_repr) list
| Repr_path of type_repr list * string

and path_repr = {
    repr_path : string;
    mutable repr_repr : type_repr;
    mutable repr_level : int;
  }

type block = {
    mutable block_scanned : bool ref;
    block_tag : int;
    block_size : int;
    block_content : int array;
    mutable block_reverse : int list;
    mutable block_weight : int;
    mutable block_type : type_repr option;
  }

type mem_repr = {
    global_names : string array;
    representations :  (string, path_repr) Hashtbl.t;
  }

type heap_info = {
    binary_name : string;
    caml_globals : int array;
    mem_repr : mem_repr array;
    globals_map : string array;
    archsize : int;
  }

type heap = {
    hp_blocks : block array;
    hp_info : heap_info;
    (*
    mutable prog_name : string;
    mutable npointers : int;
    mutable nobjects : int;
    mutable global_data : int;
    mutable codepointer : int;
    mutable restart_codepointer : int;
    mutable stack : int array;
mutable roots : int list;
  *)
  }



let print_repr paths name level indent r = ()

let print_representation paths rr = ()
