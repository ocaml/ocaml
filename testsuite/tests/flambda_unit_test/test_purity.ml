open Symbol
open Abstract_identifiers
open Flambda
open Test_utils
open Flambdapurity

type pure =
  | Pure
  | Impure

let static_exn = Static_exception.create ()
let static_exn' = Static_exception.create ()

let call_fibo =
  fapply
    ~kind:(Direct fibo_fun)
    fibonacci
    [int 5]

let f_var_g_func_env =
  mark_unasigned_variable f
    (mark_pure_functions
       (ClosureFunctionSet.singleton g_func)
       empty_env)

let impure_expr () = fccall []

let tests =
  [ "pure1",
    Pure,
    empty_env,
    tuple [int 1;int 2];

    "pure2",
    Pure,
    empty_env,
    flet x (int 1) (fvar x);

    "pure3",
    Pure,
    empty_env,
    fcatch static_exn []
      (fstaticraise static_exn [])
      (int 1);

    "pure4",
    Pure,
    empty_env,
    call_fibo;

    "pure5",
    Pure,
    f_var_g_func_env,
    fapply
      ~kind:(Direct g_func)
      (fvar f)
      [int 5];

    "impure_expr",
    Impure,
    empty_env,
    impure_expr ();

    "impure1",
    Impure,
    empty_env,
    fvar x;

    "impure2",
    Impure,
    empty_env,
    fcatch static_exn []
      (fstaticraise static_exn' [])
      (int 1);

    "impure3",
    Impure,
    empty_env,
    fapply
      ~kind:(Direct g_func)
      (fvar f)
      [int 5];
  ]

let test (name, is_pure, env, expr) =
  let r = Flambdapurity.pure_expression env expr in
  let correct = match r, is_pure with
    | true, Pure
    | false, Impure -> true
    | true, Impure | false, Pure -> false in
  if not correct
  then failwith (Printf.sprintf "incorrect purity: %s" name)


let f_fun = ffun_fclos f [x] (fadd (fvar x) (int 1))

let test' =
  (flet f' f_fun
     (flet g' (ffun_fclos g [y] (fapply ~kind:(Direct g_func) (fvar g) [fvar y]))
        (flet h' (ffun_fclos h [z] (impure_expr ()))
           (flet fi' (ffun_fclos fi [a]
                        (flet b (ffun_fclos fj [v] (int 1))
                           (fapply ~kind:(Direct h_func) (fvar a) [int 1])))
              (int 2)))))

let test_functions () =
  let s = Flambdapurity.pure_functions test' in
  let l =
    ClosureFunctionSet.of_list
      (List.map Closure_function.wrap
         [ f; g; fj ])
  in
  if not (ClosureFunctionSet.equal l s)
  then
    let diff1 = ClosureFunctionSet.diff l s in
    let diff2 = ClosureFunctionSet.diff s l in
    let r =
      Format.asprintf "incorect pure_functions: %a %a"
        ClosureFunctionSet.print diff1
        ClosureFunctionSet.print diff2 in
    failwith r

let run () =
  List.iter test tests;
  test_functions ();
  Format.printf "purity passed@."
