val handle_set :
    command: (pos:int -> len:int -> string) -> 
    ?format: string -> ?selection:string -> ?type: string -> 'a widget -> unit
(* tk invocation: selection handle <icccm list> <widget> <command> *)
