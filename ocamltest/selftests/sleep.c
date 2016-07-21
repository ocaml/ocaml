#include <stdio.h>
#include <unistd.h>

int main()
{
  if (sleep(15) == -1) perror("sleep");
  return 0;
}
