make_symlist_closure_1709
     61: (    0) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    1) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    2) anon-fn[arg.ml:61,28--52]_1718 -> Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    3) anon-fn[arg.ml:61,28--52]_1718 -> Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    4) anon-fn[arg.ml:61,28--52]_1718 -> Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    5) anon-fn[arg.ml:61,28--52]_1718 -> Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (    8) List.fold_left_2117 -> fold_left_3387: function obviously too large
     61: (   15) List.fold_left_2117 -> fold_left_3387: inlined (copying body (unconditionally))
     61: (   17) List.fold_left_2117: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=13,functor=false,branch_depth=1}=no))
     61: (   18) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     61: (   19) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
     83: (    9) List.fold_left_2117 -> fold_left_3387 -> fold_left_3400: function obviously too large
     83: (   11) List.fold_left_2117 -> fold_left_3400 -> fold_left_3387 -> fold_left_3400: function obviously too large
     83: (   13) List.fold_left_2117 -> fold_left_3400 -> fold_left_3387 -> fold_left_3400: function obviously too large
     83: (   14) List.fold_left_2117 -> fold_left_3387 -> fold_left_3400: function obviously too large
     83: (   16) List.fold_left_2117 -> fold_left_3387 -> fold_left_3400: function obviously too large
     86: (    6) List.fold_left_2117 -> fold_left_3387 -> anon-fn[arg.ml:61,28--52]_1718: function obviously too large
     86: (    7) List.fold_left_2117 -> fold_left_3387 -> fold_left_3387: function obviously too large
     86: (   10) List.fold_left_2117 -> fold_left_3400 -> anon-fn[arg.ml:61,28--52]_1718: function obviously too large
     86: (   12) List.fold_left_2117 -> fold_left_3400 -> fold_left_3387: inlined (copying body (unconditionally))

usage_string_closure_1969
     25: (    1) usage_b_closure_1932 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (    6) usage_b_closure_1932 -> Printf.kbprintf_closure_1402: function obviously too large
     65: (   11) Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     65: (   13) Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     90: (    2) usage_b_closure_1932 -> add_help_closure_1865: function obviously too large
     90: (    3) usage_b_closure_1932 -> partial_fun_4218 -> print_spec_closure_1755: function obviously too large
     90: (    4) usage_b_closure_1932 -> List.iter_2073: function obviously too large
     90: (    7) usage_b_closure_1932 -> add_help_closure_1865: function obviously too large
     90: (    8) usage_b_closure_1932 -> partial_fun_4243 -> print_spec_closure_1755: function obviously too large
     90: (    9) usage_b_closure_1932 -> List.iter_2073: function obviously too large
     94: (    0) Buffer.create_closure_1548: inlined (copying body ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=22,eval_size=-18,eval_benefit=31,functor=false,branch_depth=0}=yes))
     95: (    5) usage_b_closure_1932: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=24,eval_size=-20,eval_benefit=8,functor=false,branch_depth=0}=no))
     95: (   10) usage_b_closure_1932: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=24,eval_size=-20,eval_benefit=8,functor=false,branch_depth=0}=no))
     96: (   12) Buffer.contents_closure_1575: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=7,eval_size=-3,eval_benefit=5,functor=false,branch_depth=0}=yes))

parse_closure_2683
     18: (    5) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5016 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (    6) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5016 -> Printf.anon-fn_1463: function obviously too large
     18: (    7) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (    9) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5035 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   10) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5035 -> Printf.anon-fn_1463: function obviously too large
     18: (   11) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   13) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5055 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   14) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5055 -> Printf.anon-fn_1463: function obviously too large
     18: (   15) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   17) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5074 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   18) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5074 -> Printf.anon-fn_1463: function obviously too large
     18: (   19) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   27) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5118 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   28) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5118 -> Printf.anon-fn_1463: function obviously too large
     18: (   29) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   31) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5137 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   32) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5137 -> Printf.anon-fn_1463: function obviously too large
     18: (   33) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   35) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5157 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   36) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5157 -> Printf.anon-fn_1463: function obviously too large
     18: (   37) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   39) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5176 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   40) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5176 -> Printf.anon-fn_1463: function obviously too large
     18: (   41) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     24: (    3) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: function obviously too large
     24: (    8) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   12) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   16) Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   20) Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   25) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: function obviously too large
     24: (   30) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   34) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   38) Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   42) Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
    214: (    0) parse_argv_closure_2663 -> parse_argv_dynamic_closure_2018: function obviously too large
    214: (    2) parse_argv_closure_2663 -> parse_argv_dynamic_closure_2018: function obviously too large
    219: (    1) parse_argv_closure_2663: inlined (copying body ({benefit={call=1,alloc=0,prim=2,branch=1},orig_size=4,new_size=18,eval_size=-14,eval_benefit=21,functor=false,branch_depth=0}=yes))
    221: (    4) Printf.eprintf_closure_1517: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=2}=yes))
    221: (   22) Pervasives.exit_closure_2919: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=2}=no))
    221: (   24) Pervasives.exit_closure_2919: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=2}=no))
    222: (   26) Printf.printf_closure_1505: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=3}=yes))
    222: (   44) Pervasives.exit_closure_2919: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=3}=no))
    222: (   46) Pervasives.exit_closure_2919: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=3}=no))
    529: (   21) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   23) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   43) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   45) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large

parse_argv_dynamic_closure_2018
     20: (    3) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4414 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (    4) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4414 -> Printf.anon-fn_1479: function obviously too large
     20: (    5) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (    7) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4433 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (    8) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4433 -> Printf.anon-fn_1479: function obviously too large
     20: (    9) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   11) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4453 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   12) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4453 -> Printf.anon-fn_1479: function obviously too large
     20: (   13) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   15) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4472 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   16) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4472 -> Printf.anon-fn_1479: function obviously too large
     20: (   17) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   21) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4498 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   22) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4498 -> Printf.anon-fn_1479: function obviously too large
     20: (   23) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   25) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4517 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   26) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4517 -> Printf.anon-fn_1479: function obviously too large
     20: (   27) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   29) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4537 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   30) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4537 -> Printf.anon-fn_1479: function obviously too large
     20: (   31) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   33) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4556 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   34) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4556 -> Printf.anon-fn_1479: function obviously too large
     20: (   35) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   39) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4582 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   40) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4582 -> Printf.anon-fn_1479: function obviously too large
     20: (   41) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   43) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4601 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   44) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4601 -> Printf.anon-fn_1479: function obviously too large
     20: (   45) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   47) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4621 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   48) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4621 -> Printf.anon-fn_1479: function obviously too large
     20: (   49) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   51) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4640 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   52) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4640 -> Printf.anon-fn_1479: function obviously too large
     20: (   53) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   57) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4668 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   58) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4668 -> Printf.anon-fn_1479: function obviously too large
     20: (   59) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   61) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4687 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   62) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4687 -> Printf.anon-fn_1479: function obviously too large
     20: (   63) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   65) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4707 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   66) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4707 -> Printf.anon-fn_1479: function obviously too large
     20: (   67) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   69) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4726 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   70) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4726 -> Printf.anon-fn_1479: function obviously too large
     20: (   71) stop_closure_2048 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     25: (    1) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (    6) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   10) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   14) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   18) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   19) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   24) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   28) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   32) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   36) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   37) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   42) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   46) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   50) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   54) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=1}=no))
     25: (   55) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   60) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   64) stop_closure_2048 -> Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   68) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   72) stop_closure_2048 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   73) stop_closure_2048 -> usage_b_closure_1932 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   78) stop_closure_2048 -> usage_b_closure_1932 -> Printf.kbprintf_closure_1402: function obviously too large
     61: (   92) treat_action_2305 -> make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (   93) treat_action_2305 -> make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_4819 -> Pervasives.^_closure_2002: function obviously too large
     61: (   94) treat_action_2305 -> make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_4819 -> Pervasives.^_closure_2002: function obviously too large
     61: (   95) treat_action_2305 -> make_symlist_closure_1709 -> List.fold_left_2117: function obviously too large
     61: (   96) treat_action_2305 -> make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (   98) treat_action_2305 -> make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (   99) treat_action_2305 -> make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_4845 -> Pervasives.^_closure_2002: function obviously too large
     61: (  100) treat_action_2305 -> make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_4845 -> Pervasives.^_closure_2002: function obviously too large
     61: (  101) treat_action_2305 -> make_symlist_closure_1709 -> List.fold_left_2117: function obviously too large
     61: (  102) treat_action_2305 -> make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     65: (   83) stop_closure_2048 -> Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     65: (   85) stop_closure_2048 -> Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     65: (   86) stop_closure_2048 -> Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     65: (   88) stop_closure_2048 -> Buffer.contents_closure_1575 -> Bytes.sub_closure_1692: function obviously too large
     73: (  109) treat_action_2305 -> List.iter_2073 -> iter_4901 -> iter_4924: function obviously too large
     73: (  111) treat_action_2305 -> List.iter_2073 -> iter_4924 -> iter_4901 -> iter_4924: function obviously too large
     73: (  113) treat_action_2305 -> List.iter_2073 -> iter_4924 -> iter_4901 -> iter_4924: function obviously too large
     73: (  114) treat_action_2305 -> List.iter_2073 -> iter_4901 -> iter_4924: function obviously too large
     73: (  116) treat_action_2305 -> List.iter_2073 -> iter_4901 -> iter_4924: function obviously too large
     75: (  106) treat_action_2305 -> List.iter_2073 -> iter_4901 -> treat_action_2305: function obviously too large
     75: (  107) treat_action_2305 -> List.iter_2073 -> iter_4901 -> iter_4901: function obviously too large
     75: (  110) treat_action_2305 -> List.iter_2073 -> iter_4924 -> treat_action_2305: function obviously too large
     75: (  112) treat_action_2305 -> List.iter_2073 -> iter_4924 -> iter_4901: inlined (copying body (unconditionally))
     90: (   74) stop_closure_2048 -> usage_b_closure_1932 -> add_help_closure_1865: function obviously too large
     90: (   75) stop_closure_2048 -> usage_b_closure_1932 -> partial_fun_4753 -> print_spec_closure_1755: function obviously too large
     90: (   76) stop_closure_2048 -> usage_b_closure_1932 -> List.iter_2073: function obviously too large
     90: (   79) stop_closure_2048 -> usage_b_closure_1932 -> add_help_closure_1865: function obviously too large
     90: (   80) stop_closure_2048 -> usage_b_closure_1932 -> partial_fun_4778 -> print_spec_closure_1755: function obviously too large
     90: (   81) stop_closure_2048 -> usage_b_closure_1932 -> List.iter_2073: function obviously too large
    107: (    0) Buffer.create_closure_1548: inlined (copying body ({benefit={call=1,alloc=0,prim=2,branch=2},orig_size=4,new_size=22,eval_size=-18,eval_benefit=31,functor=false,branch_depth=0}=yes))
    115: (   56) stop_closure_2048 -> Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=2}=yes))
    117: (   20) stop_closure_2048 -> Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=1}=yes))
    119: (   38) stop_closure_2048 -> Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=1}=yes))
    122: (    2) stop_closure_2048 -> Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=1}=yes))
    124: (   77) stop_closure_2048 -> usage_b_closure_1932: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=24,eval_size=-20,eval_benefit=8,functor=false,branch_depth=0}=no))
    124: (   82) stop_closure_2048 -> usage_b_closure_1932: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=24,eval_size=-20,eval_benefit=8,functor=false,branch_depth=0}=no))
    126: (   84) stop_closure_2048 -> Buffer.contents_closure_1575: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=7,eval_size=-3,eval_benefit=5,functor=false,branch_depth=1}=yes))
    127: (   87) stop_closure_2048 -> Buffer.contents_closure_1575: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=7,eval_size=-3,eval_benefit=5,functor=false,branch_depth=1}=yes))
    134: (   89) assoc3_1690: did not try copying decl (did not try unrolling)
    135: (   90) stop_closure_2048: function obviously too large
    142: (  119) treat_action_2305 -> Pervasives.bool_of_string_closure_2055: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=3}=no))
    142: (  121) treat_action_2305 -> Pervasives.bool_of_string_closure_2055: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=3}=no))
    154: (   91) treat_action_2305 -> List.mem_2381: did not try copying decl (did not try unrolling)
    158: (  104) treat_action_2305 -> Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=4}=no))
    158: (  105) treat_action_2305 -> Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=4}=no))
    159: (   97) treat_action_2305 -> make_symlist_closure_1709: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=25,eval_size=-21,eval_benefit=5,functor=false,branch_depth=4}=no))
    159: (  103) treat_action_2305 -> make_symlist_closure_1709: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=25,eval_size=-21,eval_benefit=5,functor=false,branch_depth=4}=no))
    193: (  108) treat_action_2305 -> List.iter_2073 -> iter_4901: function obviously too large
    193: (  115) treat_action_2305 -> List.iter_2073 -> iter_4901: inlined (copying body (unconditionally))
    193: (  117) treat_action_2305 -> List.iter_2073: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=22,eval_size=-18,eval_benefit=13,functor=false,branch_depth=2}=no))
    201: (  122) treat_action_2305: function obviously too large
    202: (  123) stop_closure_2048: function obviously too large
    203: (  124) stop_closure_2048: function obviously too large
    207: (  125) stop_closure_2048: function obviously too large
    239: (  118) treat_action_2305 -> Pervasives.bool_of_string_closure_2055 -> Pervasives.invalid_arg_closure_1911: function obviously too large
    239: (  120) treat_action_2305 -> Pervasives.bool_of_string_closure_2055 -> Pervasives.invalid_arg_closure_1911: function obviously too large

max_arg_len_closure_2872
    246: (    0) Pervasives.max_closure_1939: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=1}=no))
    246: (    1) Pervasives.max_closure_1939: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=1}=no))
    247: (    2) second_word_closure_2831: function obviously too large
    247: (    3) Pervasives.max_closure_1939: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=1}=no))
    247: (    4) Pervasives.max_closure_1939: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=1}=no))

parse_argv_closure_2663
    214: (    0) parse_argv_dynamic_closure_2018: function obviously too large

align_closure_3003
     33: (   17) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.make_closure_1623: function obviously too large
     33: (   21) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.make_closure_1623: function obviously too large
     33: (   29) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.make_closure_1623: function obviously too large
     33: (   33) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.make_closure_1623: function obviously too large
     39: (   22) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.sub_closure_1692: function obviously too large
     39: (   23) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.sub_closure_1692: function obviously too large
     39: (   34) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.sub_closure_1692: function obviously too large
     39: (   35) partial_fun_5470 -> add_padding_closure_2902 -> Bytes.sub_closure_1692: function obviously too large
     55: (   42) List.map_1999 -> map_5590 -> map_5608: function obviously too large
     55: (   44) List.map_1999 -> map_5608 -> map_5590 -> map_5608: function obviously too large
     55: (   46) List.map_1999 -> map_5608 -> map_5590 -> map_5608: function obviously too large
     55: (   47) List.map_1999 -> map_5590 -> map_5608: function obviously too large
     55: (   49) List.map_1999 -> map_5590 -> map_5608: function obviously too large
     57: (   39) List.map_1999 -> map_5590 -> partial_fun_5470: function obviously too large
     57: (   40) List.map_1999 -> map_5590 -> map_5590: function obviously too large
     57: (   43) List.map_1999 -> map_5608 -> partial_fun_5470: function obviously too large
     57: (   45) List.map_1999 -> map_5608 -> map_5590: inlined (copying body (unconditionally))
     83: (    4) List.fold_left_2117 -> fold_left_5423 -> fold_left_5434: function obviously too large
     83: (    6) List.fold_left_2117 -> fold_left_5434 -> fold_left_5423 -> fold_left_5434: function obviously too large
     83: (    8) List.fold_left_2117 -> fold_left_5434 -> fold_left_5423 -> fold_left_5434: function obviously too large
     83: (    9) List.fold_left_2117 -> fold_left_5423 -> fold_left_5434: function obviously too large
     83: (   11) List.fold_left_2117 -> fold_left_5423 -> fold_left_5434: function obviously too large
     86: (    1) List.fold_left_2117 -> fold_left_5423 -> max_arg_len_closure_2872: function obviously too large
     86: (    2) List.fold_left_2117 -> fold_left_5423 -> fold_left_5423: function obviously too large
     86: (    5) List.fold_left_2117 -> fold_left_5434 -> max_arg_len_closure_2872: function obviously too large
     86: (    7) List.fold_left_2117 -> fold_left_5434 -> fold_left_5423: inlined (copying body (unconditionally))
    257: (   15) partial_fun_5470 -> add_padding_closure_2902 -> second_word_closure_2831: function obviously too large
    257: (   27) partial_fun_5470 -> add_padding_closure_2902 -> second_word_closure_2831: function obviously too large
    258: (   16) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.max_closure_1939: function obviously too large
    258: (   28) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.max_closure_1939: function obviously too large
    259: (   18) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    259: (   19) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    259: (   30) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    259: (   31) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    261: (   20) partial_fun_5470 -> add_padding_closure_2902 -> second_word_closure_2831: function obviously too large
    261: (   32) partial_fun_5470 -> add_padding_closure_2902 -> second_word_closure_2831: function obviously too large
    270: (   24) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    270: (   25) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    270: (   36) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    270: (   37) partial_fun_5470 -> add_padding_closure_2902 -> Pervasives.^_closure_2002: function obviously too large
    274: (    0) add_help_closure_1865: function obviously too large
    275: (    3) List.fold_left_2117 -> fold_left_5423: function obviously too large
    275: (   10) List.fold_left_2117 -> fold_left_5423: inlined (copying body (unconditionally))
    275: (   12) List.fold_left_2117: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=22,eval_size=-18,eval_benefit=13,functor=false,branch_depth=0}=no))
    276: (   13) Pervasives.min_closure_1930: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=0}=no))
    276: (   14) Pervasives.min_closure_1930: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=14,eval_size=-10,eval_benefit=5,functor=false,branch_depth=0}=no))
    277: (   26) partial_fun_5470 -> add_padding_closure_2902: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=41,eval_size=-37,eval_benefit=5,functor=false,branch_depth=0}=no))
    277: (   38) partial_fun_5470 -> add_padding_closure_2902: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=41,eval_size=-37,eval_benefit=5,functor=false,branch_depth=0}=no))
    277: (   41) List.map_1999 -> map_5590: function obviously too large
    277: (   48) List.map_1999 -> map_5590: inlined (copying body (unconditionally))
    277: (   50) List.map_1999: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=29,eval_size=-25,eval_benefit=13,functor=false,branch_depth=0}=no))

add_help_closure_1865
     51: (    2) assoc3_1690 -> assoc3_3923 -> assoc3_3939: function obviously too large
     51: (    3) assoc3_1690 -> assoc3_3939 -> assoc3_3923 -> assoc3_3939: function obviously too large
     51: (    5) assoc3_1690 -> assoc3_3939 -> assoc3_3923 -> assoc3_3939: function obviously too large
     51: (    6) assoc3_1690 -> assoc3_3923 -> assoc3_3939: function obviously too large
     51: (    8) assoc3_1690 -> assoc3_3923 -> assoc3_3939: function obviously too large
     51: (   12) assoc3_1690 -> assoc3_3959 -> assoc3_3975: function obviously too large
     51: (   13) assoc3_1690 -> assoc3_3975 -> assoc3_3959 -> assoc3_3975: function obviously too large
     51: (   15) assoc3_1690 -> assoc3_3975 -> assoc3_3959 -> assoc3_3975: function obviously too large
     51: (   16) assoc3_1690 -> assoc3_3959 -> assoc3_3975: function obviously too large
     51: (   18) assoc3_1690 -> assoc3_3959 -> assoc3_3975: function obviously too large
     55: (    0) assoc3_1690 -> assoc3_3923 -> assoc3_3923: function obviously too large
     55: (    4) assoc3_1690 -> assoc3_3939 -> assoc3_3923: inlined (copying body (unconditionally))
     55: (   10) assoc3_1690 -> assoc3_3959 -> assoc3_3959: function obviously too large
     55: (   14) assoc3_1690 -> assoc3_3975 -> assoc3_3959: inlined (copying body (unconditionally))
     77: (    1) assoc3_1690 -> assoc3_3923: function obviously too large
     77: (    7) assoc3_1690 -> assoc3_3923: inlined (copying body (unconditionally))
     77: (    9) assoc3_1690: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=37,eval_size=-33,eval_benefit=13,functor=false,branch_depth=0}=no))
     81: (   11) assoc3_1690 -> assoc3_3959: function obviously too large
     81: (   17) assoc3_1690 -> assoc3_3959: inlined (copying body (unconditionally))
     81: (   19) assoc3_1690: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=37,eval_size=-33,eval_benefit=13,functor=false,branch_depth=0}=no))
     85: (   20) Pervasives.@_2132: did not try copying decl (did not try unrolling)
     85: (   21) Pervasives.@_2132: did not try copying decl (did not try unrolling)

print_spec_closure_1755
     20: (   14) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3521 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   15) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3521 -> Printf.anon-fn_1479: function obviously too large
     20: (   16) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   18) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3540 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   19) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3540 -> Printf.anon-fn_1479: function obviously too large
     20: (   20) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   22) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3560 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   23) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3560 -> Printf.anon-fn_1479: function obviously too large
     20: (   24) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   26) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3579 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   27) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3579 -> Printf.anon-fn_1479: function obviously too large
     20: (   28) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   32) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3605 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   33) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3605 -> Printf.anon-fn_1479: function obviously too large
     20: (   34) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   36) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3624 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   37) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3624 -> Printf.anon-fn_1479: function obviously too large
     20: (   38) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   40) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3644 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   41) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3644 -> Printf.anon-fn_1479: function obviously too large
     20: (   42) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   44) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3663 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   45) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_3663 -> Printf.anon-fn_1479: function obviously too large
     20: (   46) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     25: (   12) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   17) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   21) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   25) Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   29) Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   30) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   35) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   39) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   43) Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     25: (   47) Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     61: (    0) make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (    1) make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_3466 -> Pervasives.^_closure_2002: function obviously too large
     61: (    2) make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_3466 -> Pervasives.^_closure_2002: function obviously too large
     61: (    3) make_symlist_closure_1709 -> List.fold_left_2117: function obviously too large
     61: (    4) make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (    6) make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     61: (    7) make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_3492 -> Pervasives.^_closure_2002: function obviously too large
     61: (    8) make_symlist_closure_1709 -> anon-fn[arg.ml:61,28--52]_3492 -> Pervasives.^_closure_2002: function obviously too large
     61: (    9) make_symlist_closure_1709 -> List.fold_left_2117: function obviously too large
     61: (   10) make_symlist_closure_1709 -> Pervasives.^_closure_2002: function obviously too large
     68: (    5) make_symlist_closure_1709: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=25,eval_size=-21,eval_benefit=5,functor=false,branch_depth=2}=no))
     68: (   11) make_symlist_closure_1709: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=25,eval_size=-21,eval_benefit=5,functor=false,branch_depth=2}=no))
     68: (   13) Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=2}=yes))
     70: (   31) Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=2}=yes))

usage_closure_1990
     18: (    8) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4310 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (    9) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4310 -> Printf.anon-fn_1463: function obviously too large
     18: (   10) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   12) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4329 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   13) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4329 -> Printf.anon-fn_1463: function obviously too large
     18: (   14) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   16) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4349 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   17) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4349 -> Printf.anon-fn_1463: function obviously too large
     18: (   18) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   20) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4368 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   21) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_4368 -> Printf.anon-fn_1463: function obviously too large
     18: (   22) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     24: (    6) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: function obviously too large
     24: (   11) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     24: (   15) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     24: (   19) Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     24: (   23) Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     65: (    1) usage_string_closure_1969 -> Bytes.sub_closure_1692: function obviously too large
     65: (    4) usage_string_closure_1969 -> Bytes.sub_closure_1692: function obviously too large
     95: (    0) usage_string_closure_1969 -> usage_b_closure_1932: function obviously too large
     95: (    3) usage_string_closure_1969 -> usage_b_closure_1932: function obviously too large
    100: (    2) usage_string_closure_1969: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=34,eval_size=-30,eval_benefit=8,functor=false,branch_depth=0}=no))
    100: (    5) usage_string_closure_1969: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=34,eval_size=-30,eval_benefit=8,functor=false,branch_depth=0}=no))
    100: (    7) Printf.eprintf_closure_1517: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))

parse_dynamic_closure_2757
     18: (    3) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5230 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (    4) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5230 -> Printf.anon-fn_1463: function obviously too large
     18: (    5) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (    7) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5249 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (    8) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5249 -> Printf.anon-fn_1463: function obviously too large
     18: (    9) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   11) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5269 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   12) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5269 -> Printf.anon-fn_1463: function obviously too large
     18: (   13) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   15) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5288 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   16) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5288 -> Printf.anon-fn_1463: function obviously too large
     18: (   17) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   25) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5332 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   26) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5332 -> Printf.anon-fn_1463: function obviously too large
     18: (   27) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   29) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5351 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   30) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5351 -> Printf.anon-fn_1463: function obviously too large
     18: (   31) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   33) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5371 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   34) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5371 -> Printf.anon-fn_1463: function obviously too large
     18: (   35) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     18: (   37) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5390 -> CamlinternalFormat.output_acc_82692: function obviously too large
     18: (   38) Printf.kfprintf_closure_1372 -> anon-fn[printf.ml:18,14--50]_5390 -> Printf.anon-fn_1463: function obviously too large
     18: (   39) Printf.kfprintf_closure_1372 -> CamlinternalFormat.make_printf_81453: function obviously too large
     24: (    1) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: function obviously too large
     24: (    6) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   10) Printf.eprintf_closure_1517 -> Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   14) Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   18) Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=2}=no))
     24: (   23) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: function obviously too large
     24: (   28) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   32) Printf.printf_closure_1505 -> Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   36) Printf.kfprintf_closure_1372: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
     24: (   40) Printf.kfprintf_closure_1372: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=3}=no))
    227: (    0) parse_argv_dynamic_closure_2018: function obviously too large
    229: (    2) Printf.eprintf_closure_1517: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=2}=yes))
    229: (   20) Pervasives.exit_closure_2919: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=2}=no))
    229: (   22) Pervasives.exit_closure_2919: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=2}=no))
    230: (   24) Printf.printf_closure_1505: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=3}=yes))
    230: (   42) Pervasives.exit_closure_2919: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=3}=no))
    230: (   44) Pervasives.exit_closure_2919: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=18,eval_size=-14,eval_benefit=5,functor=false,branch_depth=3}=no))
    529: (   19) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   21) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   41) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large
    529: (   43) Pervasives.exit_closure_2919 -> Pervasives.do_at_exit_closure_2902: function obviously too large

add_padding_closure_2902
     33: (    3) String.make_closure_1548 -> Bytes.make_closure_1623: function obviously too large
     33: (    5) String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=1}=no))
     33: (    6) String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=1}=no))
     33: (   12) String.make_closure_1548 -> Bytes.make_closure_1623: function obviously too large
     33: (   14) String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=2}=no))
     33: (   15) String.make_closure_1548 -> Bytes.make_closure_1623: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=21,eval_size=-17,eval_benefit=5,functor=false,branch_depth=2}=no))
     39: (   16) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
     39: (   18) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
     39: (   19) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
     39: (   21) String.sub_closure_1592 -> Bytes.sub_closure_1692: function obviously too large
    257: (    0) second_word_closure_2831: function obviously too large
    258: (    1) Pervasives.max_closure_1939: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=16,eval_size=-12,eval_benefit=5,functor=false,branch_depth=1}=no))
    258: (    2) Pervasives.max_closure_1939: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=16,eval_size=-12,eval_benefit=5,functor=false,branch_depth=1}=no))
    258: (    4) String.make_closure_1548: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=1}=yes))
    259: (    7) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
    259: (    8) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=1}=no))
    259: (    9) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=1}=no))
    259: (   10) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=1,branch=0},orig_size=4,new_size=40,eval_size=-36,eval_benefit=8,functor=false,branch_depth=1}=no))
    261: (   11) second_word_closure_2831: function obviously too large
    267: (   13) String.make_closure_1548: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=2}=yes))
    268: (   17) String.sub_closure_1592: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=2}=yes))
    269: (   20) String.sub_closure_1592: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=2}=yes))
    270: (   22) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=2}=no))
    270: (   23) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=2}=no))
    270: (   24) Pervasives.^_closure_2002: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=2}=no))
    270: (   25) Pervasives.^_closure_2002: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=44,eval_size=-40,eval_benefit=5,functor=false,branch_depth=2}=no))

usage_b_closure_1932
     20: (    2) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4012 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (    3) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4012 -> Printf.anon-fn_1479: function obviously too large
     20: (    4) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (    6) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4031 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (    7) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4031 -> Printf.anon-fn_1479: function obviously too large
     20: (    8) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   10) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4051 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   11) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4051 -> Printf.anon-fn_1479: function obviously too large
     20: (   12) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     20: (   14) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4070 -> CamlinternalFormat.bufput_acc_82785: function obviously too large
     20: (   15) Printf.kbprintf_closure_1402 -> anon-fn[printf.ml:20,14--50]_4070 -> Printf.anon-fn_1479: function obviously too large
     20: (   16) Printf.kbprintf_closure_1402 -> CamlinternalFormat.make_printf_81453: function obviously too large
     25: (    0) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (    5) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     25: (    9) Printf.bprintf_closure_1473 -> Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     25: (   13) Printf.kbprintf_closure_1402: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     25: (   17) Printf.kbprintf_closure_1402: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=15,eval_size=-11,eval_benefit=5,functor=false,branch_depth=0}=no))
     25: (   20) partial_fun_4081 -> print_spec_closure_1755 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   21) partial_fun_4081 -> print_spec_closure_1755 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   24) partial_fun_4081 -> print_spec_closure_1755 -> Printf.kbprintf_closure_1402: function obviously too large
     25: (   25) partial_fun_4081 -> print_spec_closure_1755 -> Printf.kbprintf_closure_1402: function obviously too large
     68: (   19) partial_fun_4081 -> print_spec_closure_1755 -> make_symlist_closure_1709: function obviously too large
     68: (   23) partial_fun_4081 -> print_spec_closure_1755 -> make_symlist_closure_1709: function obviously too large
     73: (   30) List.iter_2073 -> iter_4147 -> iter_4164: function obviously too large
     73: (   32) List.iter_2073 -> iter_4164 -> iter_4147 -> iter_4164: function obviously too large
     73: (   34) List.iter_2073 -> iter_4164 -> iter_4147 -> iter_4164: function obviously too large
     73: (   35) List.iter_2073 -> iter_4147 -> iter_4164: function obviously too large
     73: (   37) List.iter_2073 -> iter_4147 -> iter_4164: function obviously too large
     75: (   27) List.iter_2073 -> iter_4147 -> partial_fun_4081: function obviously too large
     75: (   28) List.iter_2073 -> iter_4147 -> iter_4147: function obviously too large
     75: (   31) List.iter_2073 -> iter_4164 -> partial_fun_4081: function obviously too large
     75: (   33) List.iter_2073 -> iter_4164 -> iter_4147: inlined (copying body (unconditionally))
     89: (    1) Printf.bprintf_closure_1473: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=4,eval_size=0,eval_benefit=5,functor=false,branch_depth=0}=yes))
     90: (   18) add_help_closure_1865: function obviously too large
     90: (   22) partial_fun_4081 -> print_spec_closure_1755: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=29,eval_size=-25,eval_benefit=5,functor=false,branch_depth=0}=no))
     90: (   26) partial_fun_4081 -> print_spec_closure_1755: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=29,eval_size=-25,eval_benefit=5,functor=false,branch_depth=0}=no))
     90: (   29) List.iter_2073 -> iter_4147: function obviously too large
     90: (   36) List.iter_2073 -> iter_4147: inlined (copying body (unconditionally))
     90: (   38) List.iter_2073: tried but failed (copying decl (did not try unrolling, {benefit={call=2,alloc=0,prim=1,branch=0},orig_size=4,new_size=22,eval_size=-18,eval_benefit=13,functor=false,branch_depth=0}=no))

assoc3_1690
     55: (    0) assoc3_1690: did not try copying decl (did not try unrolling)

second_word_closure_2831
    100: (    1) String.index_closure_1933 -> Bytes.index_closure_2618: function obviously too large
    100: (    4) String.index_closure_1933 -> Bytes.index_closure_2618: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=0}=no))
    100: (    6) String.index_closure_1933 -> Bytes.index_closure_2618: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=0}=no))
    220: (    3) String.index_closure_1933 -> Bytes.index_closure_2618 -> Bytes.index_rec_2597: function obviously too large
    220: (    5) String.index_closure_1933 -> Bytes.index_closure_2618 -> Bytes.index_rec_2597: function obviously too large
    237: (    0) loop_2838 -> loop_2838: did not try copying decl (did not try unrolling)
    240: (    2) String.index_closure_1933: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=5,eval_size=-1,eval_benefit=5,functor=false,branch_depth=0}=yes))
    240: (    7) loop_2838: did not try copying decl (did not try unrolling)

# vim:fdm=expr:filetype=plain:foldexpr=getline(v\:lnum)=~'^\\s*$'&&getline(v\:lnum+1)=~'\\S'?'<1'\:1
