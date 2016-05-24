/*
 Show that under Linux the core file is dumped in the directory
 where the process is at crash time, rather than in its initial
 current directory.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{
  char *p = NULL;
  if ( chdir("/tmp") < 0) perror("chdir");
  *p = 'a';
  return 0;
}
