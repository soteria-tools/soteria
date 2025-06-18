int main()
{
  int i = 42;
  int o = 0;

  int n = (i + 3) / 4;
  switch (i % 4)
  {
  case 0:
    do
    {
      o++;
    case 3:
      o++;
    case 2:
      o++;
    case 1:
      o++;
      --n;
    } while (n > 0);
  }

  return o;
}
