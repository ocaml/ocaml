| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `find_replace hashtbl` | 183.5 ± 2.8 | 178.8 | 189.7 | 1.00 |
| `find_replace hashtbl2` | 205.3 ± 2.9 | 199.1 | 209.6 | 1.12 ± 0.02 |

<!-- SIZE=1023 FIND=0 REPLACE=150_000 ITERATIONS=50 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "SIZE=1023 FIND=0 REPLACE=150_000 ITERATIONS=50 IMPL={impl}       FUNCTION=find_replace ./hashtbl_vs_hashtbl2.exe"       --command-name "find_replace {impl}" --export-markdown replace.md -->
