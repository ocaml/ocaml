type ('t1, 't2, 't3, 't4, 't5) t1 = [
  | `C1_1 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C1_2 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C1_3 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C1_4 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C1_5 of 't1 * 't2 * 't3 * 't4 * 't5
]
and ('t1, 't2, 't3, 't4, 't5) t2 = [
  | `C2_1 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C2_2 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C2_3 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C2_4 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C2_5 of 't1 * 't2 * 't3 * 't4 * 't5
]
and ('t1, 't2, 't3, 't4, 't5) t3 = [
  | `C3_1 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C3_2 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C3_3 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C3_4 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C3_5 of 't1 * 't2 * 't3 * 't4 * 't5
]
and ('t1, 't2, 't3, 't4, 't5) t4 = [
  | `C4_1 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C4_2 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C4_3 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C4_4 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C4_5 of 't1 * 't2 * 't3 * 't4 * 't5
]
and ('t1, 't2, 't3, 't4, 't5) t5 = [
  | `C5_1 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C5_2 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C5_3 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C5_4 of 't1 * 't2 * 't3 * 't4 * 't5
  | `C5_5 of 't1 * 't2 * 't3 * 't4 * 't5
]

type 'a typed = {node: 'a; loc: int}

type ('t1, 't2, 't3, 't4, 't5) t1_typed = ('t1, 't2, 't3, 't4, 't5) t1 typed
type ('t1, 't2, 't3, 't4, 't5) t2_typed = ('t1, 't2, 't3, 't4, 't5) t2 typed
type ('t1, 't2, 't3, 't4, 't5) t3_typed = ('t1, 't2, 't3, 't4, 't5) t3 typed
type ('t1, 't2, 't3, 't4, 't5) t4_typed = ('t1, 't2, 't3, 't4, 't5) t4 typed
type ('t1, 't2, 't3, 't4, 't5) t5_typed = ('t1, 't2, 't3, 't4, 't5) t5 typed

type t1' = (t1', t2', t3', t4', t5') t1_typed
and t2' = (t1', t2', t3', t4', t5') t2_typed
and t3' = (t1', t2', t3', t4', t5') t3_typed
and t4' = (t1', t2', t3', t4', t5') t4_typed
and t5' = (t1', t2', t3', t4', t5') t5_typed
