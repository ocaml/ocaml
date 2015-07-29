What to scan eagerly
--------------------
1. Current stack
2. Registers (native code)
3. Major2Minor remembered set. Do not include remembered stacks.
4. All the younger young objects. Optimization: Is there a way to safely scan
	 only part of this young minor heap?
5. Minor2Minor remembered set. Only when the field to be scanned older than
	 promoted field. The fields that are younger will be covered by 4 without the
	 proposed optimization.
6. Local roots.

What to scan lazily
-------------------
On context switch, scan the target (when target != current) stack if the stack
is on the minor heap. If the stack is on the major heap, scan only when the
stack is dirty.

When to populate Minor2Minor remembered set
-------------------------------------------
During an assignment Op_val(o)[f] = v, if o and v are in young gen, and v was
more recently allocated than o.

\forall o,f,v. is_young(o) && is_young(v) && o > v => minor2minor(o,f)

Memory barriers
---------------
* Read barrier now only generates read faults.
* Write barrier only for remembered sets.

Questions
---------

* What is the significance of `Assert(NOT_MARKABLE == Promotedhd_hd(0));`
* Do I need to scan all of the new parts of the heap?
