{ skip = 1 }
$0 ~ STDLIB_NAMESPACE_MODULES { skip = 0 }

/:/ {
  if (skip == 1) {
    print $0
  } else {
    printf "stdlib__%s", toupper(substr($1, 1, 1)) substr($1, 2) " " $2 " " $3
    if ($4 != "")
      print " " $4
    else
      print ""
  }
}
/^[^:]+$/ {
  if (skip) {
    print $0
  } else {
    printf "    stdlib__%s", toupper(substr($1, 1, 1)) substr($1, 2)
    if ($2 != "")
      print " " $2
    else
      print ""
  }
}
