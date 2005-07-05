(* The canonical addrbook example. *)

type content = {{ [(name addr tel?)*] }}
and addrbook = {{ <addrbook>content }}
and name = {{ <name>[ PCDATA ] }}
and addr = {{ <addr>[ PCDATA ] }}
and tel = {{ <tel>[ PCDATA ] }}

let rec mkTelList (l : {{[ (name addr tel?)* ]}} ) : {{[ (name tel)* ]}} =
{{ match l with
| [ <name>n <addr>_ <tel>t rest::_* ] -> [ <name>n <tel>t ] @ (mkTelList rest)
| [ <name>_ <addr>_ rest::_* ] -> mkTelList rest
| [] -> []
}}


let e = {{ mkTelList [
		    <name>"X"
		    <addr>"XA"
		    <name>"Y"
		    <addr>"YA"
		    <tel>"YT"
		    <name>"Z"
		    <addr>"ZA"
		  ] }}

let () = 
  Format.fprintf Format.std_formatter "%a@."
    Cduce_types.Value.print 
    (Obj.magic e)

