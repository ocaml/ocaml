type constant =
    Const_int of int
  | Const_float of string
  | Const_symbol of string
  | Const_pointer of int

type machtype_component =
    Addr
  | Int
  | Float

type machtype = machtype_component array

let typ_void = ([||] : machtype)
let typ_addr = [|Addr|]
let typ_int = [|Int|]
let typ_float = [|Float|]

let size_component = function
    Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge

let negate_comparison = function
    Ceq -> Cne | Cne -> Ceq
  | Clt -> Cge | Cle -> Cgt
  | Cgt -> Cle | Cge -> Clt

let swap_comparison = function
    Ceq -> Ceq | Cne -> Cne
  | Clt -> Cgt | Cle -> Cge
  | Cgt -> Clt | Cge -> Cle

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Word

type operation =
    Capply of machtype
  | Cextcall of string * machtype
  | Cproj of int * int
  | Cload of machtype
  | Cloadchunk of memory_chunk
  | Calloc
  | Cstore
  | Cstorechunk of memory_chunk
  | Cmodify
  | Caddi | Csubi | Cmuli | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of comparison
  | Cadda | Csuba
  | Ccmpa of comparison
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of comparison
  | Craise

type expression =
    Cconst of constant
  | Cvar of Ident.t
  | Clet of Ident.t * expression * expression
  | Cassign of Ident.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array
  | Cwhile of expression * expression
  | Ccatch of expression * expression
  | Cexit
  | Ctrywith of expression * Ident.t * expression

type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression }

type data_item =
    Clabel of string
  | Cint8 of int
  | Cint16 of int
  | Cint of int
  | Cfloat of string
  | Caddress of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

