#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

double foo;

void access_double(p)
     double * p;
{
  foo = *p;
}

jmp_buf failure;

void sig_handler()
{
  longjmp(failure, 1);
}

main()
{
  long n[10];
  int res;
  signal(SIGSEGV, sig_handler);
  signal(SIGBUS, sig_handler);
  if(setjmp(failure) == 0) {
    access_double((double *) n);
    access_double((double *) (n+1));
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  exit(res);
}

