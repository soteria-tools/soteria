int empty(int x, int y)
{
  if (x > y)
  {
    return 1;
  }
  else
  {
    return 2;
  }
}

int main()
{
  return empty(11, 12);
}