open Rtype

val iter : (type_expr -> Obj.t -> unit) -> type_expr -> Obj.t -> unit
(** generic iterator 
   [iter f ty v] iterates blocks and values in v and applies f.
   Raising [Exit] inside [f] will surpress the further recursive iteration.
   Note that [iter] may loop infinitely if [v] contains loops.
   To avoid the inifinite loop, use [iter_with_loop] or write
   a check [v] is visited or not visited inside [f] like:
   
   let visited_block = ref [] in
   let f ty v =
     if Obj.is_block v && List.memq v !visited_block then raise Exit
     else begin
      visited_block := v :: !visited_block;
      ...
     end 
   in
 *)

val iter_with_loop : (type_expr -> Obj.t -> unit) -> type_expr -> Obj.t -> unit
(** same as [iter], but iterates each object only once. 
   But note that the visited object cache is reset inside [f], 
   therefore [f] should not use [iter_with_loop] recursively. *)

