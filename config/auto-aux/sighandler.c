#include <signal.h>

main()
{
  SIGRETURN (*old)();
  old = signal(SIGQUIT, SIG_DFL);
  return 0;
}
