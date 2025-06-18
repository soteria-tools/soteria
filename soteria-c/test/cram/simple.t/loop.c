int main()
{
  int i = 0;
  int j = 0;
  while (i < 10)
  {
    i++;
    if (i < 5)
    {
      continue;
    }
    j++;
    if (i >= 8)
    {
      break;
    }
  }
  // Should return 4, because j = 4 and i = 8
  return i - j;
}