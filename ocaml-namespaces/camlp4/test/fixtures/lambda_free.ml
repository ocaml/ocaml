open Format;

module S = Set.Make String;

type term =
  [ Lambda of string and term
  | Atom of string
  | App of term and term
  | Opt of term and option term and term
  ];

value free_vars =
  let rec fv t env free =
    match t with
    [ Lambda x t -> fv t (S.add x env) free
    | Atom x -> if S.mem x env then free else S.add x free
    | App t1 t2 -> fv t1 env (fv t2 env free)
    | Opt _ _ _ -> assert False ]
  in fun t -> fv t S.empty S.empty;

value print_set f s = do {
  fprintf f "@[<2>{ ";
  S.iter (fprintf f "%s@ ") s;
  fprintf f "}@]";
};

value t1 = Lambda "x" (App (Lambda "y" (App (Atom "y") (Atom "x"))) (Lambda "x" (Atom "x")));
value t2 = Lambda "x" (App (Lambda "y" (App (Atom "y") (Atom "x"))) (Lambda "z" (Atom "z")));
value t3 = Lambda "x" (App (Lambda "y" (App (Atom "y") (Atom "x"))) (Lambda "x" (Atom "z")));
value t4 = Lambda "a" (App (Lambda "y" (App (Atom "y") (Atom "x"))) (Lambda "x" (Atom "z")));

printf "t1: %a@." print_set (free_vars t1);
printf "t2: %a@." print_set (free_vars t2);
printf "t3: %a@." print_set (free_vars t3);
printf "t4: %a@." print_set (free_vars t4);

class fold ['accu] init =
  object (o : 'self_type)
    value accu : 'accu = init;
    method accu = accu;
    method term t =
      match t with
      [ Lambda x t -> (o#string x)#term t
      | Atom x -> o#string x
      | App t1 t2 -> (o#term t1)#term t2
      | Opt t1 ot t2 -> ((o#term t1)#option (fun o -> o#term) ot)#term t2 ];
    method string : string -> 'self_type = fun _ -> o;
    method option : ! 'a. ('self_type -> 'a -> 'self_type) -> option 'a -> 'self_type =
      fun f opt ->
        match opt with
        [ None -> o
        | Some x -> f o x ];
  end;

class fold_atoms ['accu] f init =
  object (o : 'self_type)
    inherit fold ['accu] init as super;
    method term t =
      match t with
      [ Atom x -> {< accu = f x accu >}
      | _ -> super#term t ];
  end;

value t5 = Opt (Atom "a") (Some (Atom "b")) (Atom "c");

value atoms = ((new fold_atoms S.add S.empty)#term t5)#accu;

printf "atoms: %a@." print_set atoms;
