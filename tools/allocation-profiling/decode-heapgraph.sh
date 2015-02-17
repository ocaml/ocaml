#!/bin/bash

set -eu -o pipefail -x

nodes_in="$1"
edges_in="$2"
exe="$3"

from=$(mktemp)
to=$(mktemp)
graph=$(mktemp)
body=$(mktemp)
nodes=$(mktemp)
edges=$(mktemp)
edges_from=$(mktemp)
edges_to=$(mktemp)
nodes_tmp=$(mktemp)
edges_from_tmp=$(mktemp)
edges_to_tmp=$(mktemp)
nodes_in_order_of_num_allocs=$(mktemp)

cat $nodes_in | awk -F, '{print "inf line*" $1}' > $nodes_tmp
cat $edges_in | awk -F, '{print "inf line*" $1}' > $edges_from_tmp
cat $edges_in | awk -F, '{print "inf line*" $2}' > $edges_to_tmp

decode ()
{
  input="$1"
  gdb $exe --batch -x $input | sed 's/" starts.*//' | sed 's/"//' | sed 's/No line number information available for address //' | sed 's/Line \([0-9]*\) of \(.*\)/\2:\1/' | sed 's/^/"/' | sed 's/$/"/' | sed 's/ /_/g'
}

decode $nodes_tmp > $nodes
decode $edges_from_tmp > $edges_from
decode $edges_to_tmp > $edges_to

cat $nodes | sort | uniq -c | sort -n | awk '{print $2}' > $nodes_in_order_of_num_allocs

max=100

paste $edges_from $edges_to | \
  sort | uniq -c | tail -${max} | awk '{print $2 " -> " $3 " [label=" $1 ",penwidth=" 1+log($1) "]"}' > $edges

echo "digraph heap {" > $graph

num_lines=0
num_lines=$(cat $nodes_in_order_of_num_allocs | { while read line; do
  if grep $line $edges > /dev/null; then
    num_lines=$(( $num_lines + 1 ))
  fi
done; echo $num_lines ;  })

line_number=0
cat $nodes_in_order_of_num_allocs | while read line; do
  if grep $line $edges > /dev/null; then
    color=$(( 255 - $line_number * 255 / $num_lines ))
    color_hex=$(printf "%02x" $color)
    echo "$line [shape=box,style=filled,fillcolor=\"#ff${color_hex}33\"]" >> $graph
    line_number=$(( $line_number + 1 ))
  fi
done

cat $edges >> $graph
echo "}" >> $graph

cat $graph | tee /tmp/blah

neato -Tpdf -o heapgraph.pdf -Goverlap=false $graph

#rm -f $from $to $graph $body $edges
