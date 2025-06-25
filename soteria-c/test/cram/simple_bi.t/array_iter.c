int test(int *p, int len)
{
  int sum = 0;
  for (int i = 0; i < len; i++)
  {
    sum += p[i];
  }
  return sum;
}