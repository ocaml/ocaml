#include <stdio.h>

#ifdef SORT

long cmpint(i, j)
     long * i, * j;
{
  return *i - *j;
}

#endif

int main(argc, argv)
     int argc;
     char ** argv;
{
#ifdef UNIT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN));
  }
#else
  if (argc < 2) {
    fprintf(stderr, "Usage: %s [int arg]\n", argv[0]);
    exit(2);
  }
#ifdef INT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef INT_FLOAT
  { extern double FUN();
    extern double call_gen_code();
    printf("%f\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef SORT
  { extern void FUN();
    extern void call_gen_code();
    long n;
    long * a, * b;
    long i;

    srandom(argc >= 3 ? atoi(argv[2]) : time((char *) 0));
    n = atoi(argv[1]);
    a = (long *) malloc(n * sizeof(long));
    for (i = 0 ; i < n; i++) a[i] = random() & 0xFFF;
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%d ", a[i]); printf("\n");
#endif
    b = (long *) malloc(n * sizeof(long));
    for (i = 0; i < n; i++) b[i] = a[i];
    call_gen_code(FUN, 0, n-1, a);
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%d ", a[i]); printf("\n");
#endif
    qsort(b, n, sizeof(long), cmpint);
    for (i = 0; i < n; i++) {
      if (a[i] != b[i]) { printf("Bug!\n"); return 2; }
    }
    printf("OK\n");
  }
#endif
#endif
  return 0;
}
