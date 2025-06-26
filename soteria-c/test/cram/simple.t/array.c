void __assert__(int);

int main()
{
  char *c = "abcd";
  __assert__(c[2] == 'c');
}