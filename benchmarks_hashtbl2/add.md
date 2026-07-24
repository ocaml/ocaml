| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `add hashtbl` | 336.2 ± 5.1 | 328.3 | 345.0 | 1.00 |
| `add hashtbl2` | 424.8 ± 4.6 | 416.1 | 433.0 | 1.26 ± 0.02 |

<!-- SIZE=1000 ITERATIONS=10_000 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "SIZE=1000 ITERATIONS=10_000 IMPL={impl}       FUNCTION=add ./hashtbl_vs_hashtbl2.exe"       --command-name "add {impl}" --export-markdown add.md -->
