int __nondet__();

int main()
{
  int x = __nondet__();
  int y = __nondet__();

  if (x >= y)
  {
    return 1;
  }

  while (x < y)
  {
    x++;
  }
}