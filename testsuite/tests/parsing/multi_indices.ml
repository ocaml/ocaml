(* TEST
    flags = "-dsource"
    * expect
*)

module A = Bigarray.Genarray
[%%expect {|

module A = Bigarray.Genarray;;
module A = Bigarray.Genarray
|}]

let (.%{;..}<-) = A.set
let (.%{;..}) = A.get
[%%expect {|

let (.%{;..}<-) = A.set;;
val ( .%{;..}<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit = <fun>

let (.%{;..}) = A.get;;
val ( .%{;..} ) : ('a, 'b, 'c) A.t -> int array -> 'a = <fun>
|}]

let (.![;..]<-) = A.set
let (.![;..]) a n =
  (* Check the ordering of indices *)
  Format.printf "indices: @[[|%a|]@]@."
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       Format.pp_print_int) (Array.to_list n);
  A.get a n
[%%expect {|

let (.![;..]<-) = A.set;;
val ( .![;..]<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit = <fun>

let (.![;..]) a n =
  Format.printf "indices: @[[|%a|]@]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf -> fun () -> Format.fprintf ppf ";@ ")
       Format.pp_print_int) (Array.to_list n);
  A.get a n;;
val ( .![;..] ) : ('a, 'b, 'c) A.t -> int array -> 'a = <fun>
|}]

let (.?(;..)<-) = A.set
let (.?(;..)) = A.get
[%%expect {|

let (.?(;..)<-) = A.set;;
val ( .?(;..)<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit = <fun>

let (.?(;..)) = A.get;;
val ( .?(;..) ) : ('a, 'b, 'c) A.t -> int array -> 'a = <fun>
|}]

let a = A.create Bigarray.float64 Bigarray.c_layout [|3;3;3|]
[%%expect {|

let a = A.create Bigarray.float64 Bigarray.c_layout [|3;3;3|];;
val a : (float, Bigarray.float64_elt, Bigarray.c_layout) A.t = <abstr>
|}]

;; a.![1;0;0] <- 2.
[%%expect {|

;;a.![1;0;0] <- 2.;;
- : unit = ()
|}]
;; a.?(0;1;0) <- 3.
[%%expect {|

;;a.?(0;1;0) <- 3.;;
- : unit = ()
|}]
;; a.%{0;0;1} <- 5.
[%%expect {|

;;a.%{0;0;1} <- 5.;;
- : unit = ()
|}]

;; a.![0;1;2] <- 7.;
   a.![0;1;2]
[%%expect {|

;;a.![0;1;2] <- 7.; a.![0;1;2];;
indices: [|0; 1; 2|]
- : float = 7.
|}]


let (#+) = ( +. )
[%%expect {|

let (#+) = (+.);;
val ( #+ ) : float -> float -> float = <fun>
|}]

;; a.?(1;0;0) #+ a.%{0;1;0} #+ a.![0;0;1]
[%%expect {|

;;((a.?(1;0;0)) #+ (a.%{0;1;0})) #+ (a.![0;0;1]);;
indices: [|0; 0; 1|]
- : float = 10.
|}]

let (.??[]) () () = ()
;; ().??[(();())]
  [%%expect {|

let (.??[]) () () = ();;
val ( .??[] ) : unit -> unit -> unit = <fun>

;;().??[((); ())];;
- : unit = ()
|}]

module M = struct
  let (.%?(;..)) = A.get
  let (.%?(;..)<-) = A.set
  let (.%![;..]) = A.get
  let (.%![;..]<-) = A.set
  let (.%%{;..}) = A.get
  let (.%%{;..}<-) = A.set
end

;; a.M.%![1;0;0] <- 7.
[%%expect {|

module M =
  struct
    let (.%?(;..)) = A.get
    let (.%?(;..)<-) = A.set
    let (.%![;..]) = A.get
    let (.%![;..]<-) = A.set
    let (.%%{;..}) = A.get
    let (.%%{;..}<-) = A.set
  end;;
module M :
  sig
    val ( .%?(;..) ) : ('a, 'b, 'c) A.t -> int array -> 'a
    val ( .%?(;..)<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit
    val ( .%![;..] ) : ('a, 'b, 'c) A.t -> int array -> 'a
    val ( .%![;..]<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit
    val ( .%%{;..} ) : ('a, 'b, 'c) A.t -> int array -> 'a
    val ( .%%{;..}<- ) : ('a, 'b, 'c) A.t -> int array -> 'a -> unit
  end

;;a.M.%![1;0;0] <- 7.;;
- : unit = ()
|}]
;; a.M.%?(0;1;0) <- 11.
[%%expect {|

;;a.M.%?(0;1;0) <- 11.;;
- : unit = ()
|}]
;; a.M.%%{0;0;1} <- 13.
[%%expect {|

;;a.M.%%{0;0;1} <- 13.;;
- : unit = ()
|}]

;; a.M.%?(1;0;0) #+ a.M.%%{0;1;0} #+ a.M.%![0;0;1]
[%%expect {|

;;((a.M.%?(1;0;0)) #+ (a.M.%%{0;1;0})) #+ (a.M.%![0;0;1]);;
- : float = 31.
|}]
