| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `add_remove hashtbl` | 399.0 ± 6.2 | 387.7 | 409.3 | 1.00 |
| `add_remove hashtbl2` | 605.2 ± 9.3 | 592.8 | 618.8 | 1.52 ± 0.03 |

<!-- ADD=1000 REMOVE=1000 ITERATIONS=10_000 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "ADD=1000 REMOVE=1000 ITERATIONS=10_000 IMPL={impl}       FUNCTION=add_remove ./hashtbl_vs_hashtbl2.exe"       --command-name "add_remove {impl}" --export-markdown add_remove.md -->
