## Test suite

When run as part of the test suite the memory model tests are run as follows:

  + No more than two cores are used.

  + Only forbidden tests are executed. Forbidden tests are tests
  that can invalidate the memory model.

  + Output is minimal, so as to match reference output.


## Running tests explicitly

Tests can be compiled with `make`, producing native and bytecode executable.

Tests are then executed by executing the commands `./forbidden.exe`,  `./forbidden.byt`,`publish.exe` and `publish.byt`.

The tests accepts command line options, the most interesting options are;

  + `-v`  command verbose output: counts of the final values of some test
    variables are shown. Some additional tests that may exhibit legal
    relaxations are executed.
  + `-a <n>`  Tests will run on at most <n> cores (default is 2).
    Having `<n>` close to the actual number of cores of the tested
    machine will stress its memory system.
  + Other parameters `-s <S>` and `-r <R>` are integer parameters related
    to how and how many  experiments are performed,
    see http://diy.inria.fr/doc/litmus.html#sec%3Aarch.
    Default `-s 5000 -r 20` yields acceptable running times.
    Other values such as -s 500 -r 200` are worth trying.
