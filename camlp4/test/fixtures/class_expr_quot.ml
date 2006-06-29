<:class_expr< a >>;
<:class_expr< A.a B.b >>;
<:class_expr< a [ t ] >>;
<:class_expr< virtual a >>;
<:class_expr< virtual $a$ >>;
<:class_expr< virtual $lid:a$ >>;
<:class_expr< virtual $lid:a$ [ 't ] >>;
(* <:class_expr< virtual a [ t ] >>; *)
<:class_expr< $opt:v$ a >>;
<:class_expr< $opt:v$ a [ t ] >>;
<:class_expr< $opt:v$ $a$ >>;
<:class_expr< $opt:v$ $id:a$ >>;
<:class_expr< $opt:v$ $a$ [ $t$ ] >>;
(* <:class_expr< $opt:v$ a [ $t$ ] >>; *)
(* <:class_expr< $opt:v$ a $opt:t$ >>; *)
(* <:class_expr< $opt:v$ $a$ $opt:t$ >>; *)

<:class_type< a >>;
<:class_type< a [ t ] >>;
<:class_type< virtual a >>;
<:class_type< virtual $a$ >>;
<:class_type< virtual $lid:a$ >>;
<:class_type< virtual $lid:a$ [ 't ] >>;
<:class_type< $opt:v$ a >>;
<:class_type< $opt:v$ a [ t ] >>;
<:class_type< $opt:v$ $a$ >>;
<:class_type< $opt:v$ $a$ [ $t$ ] >>;
