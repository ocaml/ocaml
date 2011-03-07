(** *)

class type doc_generator =
  object method generate : Odoc_module.t_module list -> unit end;;

module type Base = sig
    class generator : doc_generator
  end;;

type generator =
  | Html of (module Odoc_html.Html_generator)
  | Latex of (module Odoc_latex.Latex_generator)
  | Texi of (module Odoc_texi.Texi_generator)
  | Man of (module Odoc_man.Man_generator)
  | Dot of (module Odoc_dot.Dot_generator)
  | Other of (module Base)
;;

let get_minimal_generator = function
  Html m ->
    let module M = (val m : Odoc_html.Html_generator) in
    (new M.html :> doc_generator)
| Latex m ->
    let module M = (val m : Odoc_latex.Latex_generator) in
    (new M.latex :> doc_generator)
| Man m ->
    let module M = (val m : Odoc_man.Man_generator) in
    (new M.man :> doc_generator)
| Texi m ->
    let module M = (val m : Odoc_texi.Texi_generator) in
    (new M.texi :> doc_generator)
| Dot m ->
    let module M = (val m : Odoc_dot.Dot_generator) in
    (new M.dot :> doc_generator)
| Other m ->
    let module M = (val m : Base) in
    new M.generator
    ;;