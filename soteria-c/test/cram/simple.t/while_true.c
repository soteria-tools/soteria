int *malloc(__cerbty_size_t);
int __nondet__();
int __assert__(int);

int main()
{
  int y = __nondet__();
  if (y)
  {
    __assert__(0);
  }
  while (1)
  {
  }
  if (y + 1)
  {
    __assert__(0);
  }
  return 0;
}