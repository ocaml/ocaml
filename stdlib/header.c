char * runtime_name = "cslrun";
char * errmsg = "Cannot exec cslrun.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  execvp(runtime_name, argv);
  write(2, errmsg, sizeof(errmsg)-1);
  return 2;
}
