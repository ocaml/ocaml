char foo[]="\377";
main()
{
  int i;
  i = foo[0];
  exit(i != -1);
}
