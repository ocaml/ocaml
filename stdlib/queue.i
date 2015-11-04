iter_closure_1531
    136: (    0) iter_1546 -> iter_1546: did not try copying decl (did not try unrolling)
    137: (    1) iter_1546: did not try copying decl (did not try unrolling)

copy_closure_1465
    101: (    0) create_closure_1345: tried but failed (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=5,functor=false,branch_depth=1}=no))
    101: (    1) create_closure_1345: tried but failed (copying body using subfunctions ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=9,eval_size=-5,eval_benefit=5,functor=false,branch_depth=1}=no))
    116: (    2) copy_1483 -> copy_1483: did not try copying decl (did not try unrolling)
    118: (    3) copy_1483: did not try copying decl (did not try unrolling)

transfer_closure_1601
    156: (    0) clear_closure_1356: inlined (copying body ({benefit={call=1,alloc=0,prim=0,branch=0},orig_size=4,new_size=7,eval_size=-3,eval_benefit=5,functor=false,branch_depth=1}=yes))

fold_closure_1566
    149: (    0) fold_1581 -> fold_1581: did not try copying decl (did not try unrolling)
    150: (    1) fold_1581: did not try copying decl (did not try unrolling)

# vim:fdm=expr:filetype=plain:foldexpr=getline(v\:lnum)=~'^\\s*$'&&getline(v\:lnum+1)=~'\\S'?'<1'\:1
