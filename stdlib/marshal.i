from_bytes_closure_1397
     55: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     55: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     59: (    2) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=2}=no))
     59: (    3) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=2}=no))

to_buffer_closure_1324
     32: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     32: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))

total_size_closure_1385
     49: (    0) data_size_closure_1359 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     49: (    2) data_size_closure_1359 -> Pervasives.invalid_arg_closure_1911: function obviously too large
     51: (    1) data_size_closure_1359: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=34,eval_size=-30,eval_benefit=5,functor=false,branch_depth=0}=no))
     51: (    3) data_size_closure_1359: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=34,eval_size=-30,eval_benefit=5,functor=false,branch_depth=0}=no))

data_size_closure_1359
     49: (    0) Pervasives.invalid_arg_closure_1911: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))
     49: (    1) Pervasives.invalid_arg_closure_1911: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=11,eval_size=-7,eval_benefit=5,functor=false,branch_depth=1}=no))

from_string_closure_1439
     66: (    0) Bytes.anon-fn_2868: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=0,eval_size=4,eval_benefit=5,functor=false,branch_depth=0}=yes))
     66: (    1) from_bytes_closure_1397: function obviously too large

# vim:fdm=expr:filetype=plain:foldexpr=getline(v\:lnum)=~'^\\s*$'&&getline(v\:lnum+1)=~'\\S'?'<1'\:1
