int __nondet__();

int simple(int x)
{
  if (x >= 5)
  {
    if (x < 0)
    {
      return -1; // should be unreachable
    }
    return 1;
  }
  else
  {
    return 2;
  }
}

int main()
{
  int x = __nondet__(); long y = 12L;
  return simple(x);
}