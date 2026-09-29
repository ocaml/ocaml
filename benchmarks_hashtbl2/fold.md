| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `fold hashtbl` | 623.8 ± 15.6 | 603.9 | 651.5 | 1.12 ± 0.07 |
| `fold hashtbl2` | 555.5 ± 32.1 | 515.2 | 584.5 | 1.00 |

<!-- SIZE=15_000 ITERATIONS=10_000 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "SIZE=15_000 ITERATIONS=10_000 IMPL={impl}       FUNCTION=fold ./hashtbl_vs_hashtbl2.exe"       --command-name "fold {impl}" --export-markdown fold.md -->
