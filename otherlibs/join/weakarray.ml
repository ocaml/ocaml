
(*

 *** array_add weak_array ele *** retourne l'index de la position a
  laquelle a ete stocke l'element *ele* dans *weak_t*, ou leve
  l'exception * Failure _ * si aucune place n'etait disponible. (on 
  utilisera alors *array_copy* pour aggrandir le tableau).

*** array_copy weak_array size *** copie le contenu de *weak_array* dans
  un autre *'a array* de taille *size* nouvellement alloue.

*** array_free weak_array n *** libere l'emplacement *n* de *weak_array* 
  de sorte  qu'il puisse etre utilise par un *array_add*. Tant que
  cette operation n'est pas effectuee, un slot occupe ne peut pas
  etre reutilise (meme par la liberation du GC).

*** array_get weak_array n *** retourne *None* si le slot *n* est libre
  ou si le pointeur faible ne pointe plus sur l'objet (L'operation
  suivante consiste alors a liberer ce slot par *array_free weak_array n*
  )

  L'avantage de cette semantique est la rapidite des operations *array_add*
  (couplee a *array_copy* en cas de failure) , *array_get* et *array_free*.
  L'operation la plus couteuse est *set weak_t n (Some p)* quand n est 
  un element libre non ordonne (ce qui est rare, mais peut arriver. 
  Dans un tel cas, on utilisera un *'a t* de toute facon
  plutot qu'un *'a array* ).


*)

type 'a t;;

external create: int -> 'a t = "weakarray_create"
external copy: 'a t -> int -> 'a t = "weakarray_realloc";;
external add : 'a t -> 'a option -> int = "weakarray_add";;
external set : 'a t -> int -> 'a option -> unit = "weakarray_set";;
external get: 'a t -> int -> 'a option = "weakarray_get";;
external free: 'a t -> int -> unit = "weakarray_free";;
let length x = Obj.size(Obj.repr x) - 2

let ref_add array option =
  try
    add (!array) option
  with
    Failure _ ->
      array := copy (!array) 
           ((length !array)*2);
      add (!array) option
