int main()
{
#ifdef __GNUC__
  exit(0);
#else
  exit(1);
#endif
}
