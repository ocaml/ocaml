#ifdef DEBUG
#define Assert(x) if(!(x)) failed_assert(__FILE__, __LINE__)
#else
#define Assert(x)
#endif
