| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `find_replace hashtbl` | 166.3 ± 2.2 | 163.5 | 171.6 | 1.00 |
| `find_replace hashtbl2` | 174.4 ± 3.1 | 169.7 | 179.8 | 1.05 ± 0.02 |

<!-- SIZE=1023 FIND=150_000 REPLACE=0 ITERATIONS=50 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "SIZE=1023 FIND=150_000 REPLACE=0 ITERATIONS=50 IMPL={impl}       FUNCTION=find_replace ./hashtbl_vs_hashtbl2.exe"       --command-name "find_replace {impl}" --export-markdown find.md -->
