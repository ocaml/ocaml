#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>

union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
} sock_addr;

int sock_addr_len;

#ifdef __STDC__
void get_sockaddr(value);
value alloc_sockaddr(void);
value alloc_inet_addr(unsigned long);
#else
void get_sockaddr();
value alloc_sockaddr();
value alloc_inet_addr();
#endif

#define GET_INET_ADDR(v) (*((unsigned long *) (v)))
