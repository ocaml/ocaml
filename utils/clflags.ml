(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a and -lxxx files *)

let compile_only = ref false            (* -c *)
and exec_name = ref "a.out"             (* -o *)
and archive_name = ref "library.cma"    (* -o *)
and include_dirs = ref ([] : string list)(* - I *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and fast = ref false                    (* -fast *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and ccopts = ref ([] : string list)     (* -ccopt *)
and nopervasives = ref false            (* -nopervasives *)

let dump_lambda = ref false             (* -dlambda *)
and dump_instr = ref false              (* -dinstr *)

let write_lambda = ref false            (* -wlambda *)
