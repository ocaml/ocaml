| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `filter_map_inplace hashtbl` | 990.1 ± 19.1 | 963.1 | 1020.7 | 1.00 |
| `filter_map_inplace hashtbl2` | 1423.5 ± 28.8 | 1389.7 | 1476.2 | 1.44 ± 0.04 |

<!-- SIZE=1000 ITERATIONS=20_000 RATIO=50 -->
<!-- hyperfine -L impl hashtbl,hashtbl2 "SIZE=1000 ITERATIONS=20_000 RATIO=50 IMPL={impl}       FUNCTION=filter_map_inplace ./hashtbl_vs_hashtbl2.exe"       --command-name "filter_map_inplace {impl}" --export-markdown filter_map_inplace.md -->
