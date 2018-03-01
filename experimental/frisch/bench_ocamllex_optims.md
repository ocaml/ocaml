Some benchmark to evaluate the speedup to `ocamllex -ml`.

In all tests, we tokenize the `typecore.ml` file (first loaded in
memory) file using either:

  - the OCaml lexer, or

  - a simpler lexer with trivial actions (to eliminate the cost of
actions themselves, which is not under the control of ocamllex).

We run the output of:

   - `ocamllex` without the -ml flag, i.e. using tables interpreted at
runtime by the C support code

   - `ocamllex -ml`, i.e. the automaton is translated to OCaml code;
this is done on before and after the optimizations;

In adition, since it turned out that the automatic update of lex_start_p by
the generated code is quite costly, there is now logic so that
the generated code does not update this field and lex_curr_p when
lex_start_p is initially physically equal to Lexing.dummy_pos.  This is also
tested (only for the simpler lexer, since the OCaml one update the field in
its actions).

For each case, we compile the benchmark with:

   - `ocamlc`

   - `ocamlopt -inline 10`

   - `ocamlopt -inline 1000`

(flambda disabled).

The tables below show:

   - the throughput (Mb of source code tokenized
by second -- higher is better;

   - its inverse (number of milleseconds to parse one Mb) -- lower is better;

   - the allocation ratio (number of bytes allocated by the GC for each byte of source code)


Conclusions:

  - In native code, the "-ml" mode is slightly slower than the table
    mode before the optimizations, but it becomes significantly faster
    after the optimizations, obviously even more so when the
    lexer actions are trivial (throughput 58.44 -> 98.30).

  - In bytecode, the "-ml" mode is always much slower than the table
    mode, but the optimization reduce the gap is little bit.

  - Not tested here, but it is likely that the optimizations produce
    code which would be more friendly to Javascript backends
    (js_of_ocaml and Bucklescript), as they reduce quite a bit
    the number of function calls and mutations.

Note:

  - The "refill handler" mode has been lightly tested only.


OCaml lexer:

````
WITHOUT -ml flag:
  NATIVE, -inline 1000:     38.07 Mb/s      26.27 ms/Mb   alloc x    36.79
  NATIVE, -inline 10  :     35.42 Mb/s      28.23 ms/Mb   alloc x    36.79
  BYTECODE            :      7.84 Mb/s     127.54 ms/Mb   alloc x    35.48


WITH -ml flag, TRUNK:
  NATIVE, -inline 1000:     34.36 Mb/s      29.11 ms/Mb   alloc x    36.79
  NATIVE, -inline 10  :     34.12 Mb/s      29.31 ms/Mb   alloc x    36.79
  BYTECODE            :      4.08 Mb/s     244.93 ms/Mb   alloc x    35.48


WITH -ml flag, BRANCH:
  NATIVE, -inline 1000:     45.56 Mb/s      21.95 ms/Mb   alloc x    36.79
  NATIVE, -inline 10  :     43.19 Mb/s      23.15 ms/Mb   alloc x    36.79
  BYTECODE            :      4.35 Mb/s     229.91 ms/Mb   alloc x    35.48
````


Simpler lexer (trivial actions):

````
WITHOUT -ml flag:
  NATIVE, -inline 1000:     58.44 Mb/s      17.11 ms/Mb   alloc x    21.94
  NATIVE, -inline 10  :     58.24 Mb/s      17.17 ms/Mb   alloc x    21.94
  BYTECODE            :     12.63 Mb/s      79.21 ms/Mb   alloc x    21.93

WITH -ml flag, TRUNK:
  NATIVE, -inline 1000:     55.14 Mb/s      18.13 ms/Mb   alloc x    21.94
  NATIVE, -inline 10  :     50.76 Mb/s      19.70 ms/Mb   alloc x    21.94
  BYTECODE            :      5.74 Mb/s     174.22 ms/Mb   alloc x    21.93

WITH -ml flag, BRANCH:
  NATIVE, -inline 1000:     98.30 Mb/s      10.17 ms/Mb   alloc x    21.94
  NATIVE, -inline 10  :     87.16 Mb/s      11.47 ms/Mb   alloc x    21.94
  BYTECODE            :      6.48 Mb/s     154.43 ms/Mb   alloc x    21.93

WITH -ml flag, BRANCH, dummy_pos:
  NATIVE, -inline 1000:    152.68 Mb/s       6.55 ms/Mb   alloc x     1.00
  NATIVE, -inline 10  :    133.97 Mb/s       7.46 ms/Mb   alloc x     1.00
  BYTECODE            :      7.42 Mb/s     134.81 ms/Mb   alloc x     1.00
````
