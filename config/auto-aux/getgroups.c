#include <sys/types.h>
#include <sys/param.h>

#ifdef NGROUPS

int main()
{
  int gidset[NGROUPS];
  if (getgroups(NGROUPS, gidset) == -1) return 1;
  return 0;
}

#else

int main() { return 1; }

#endif
