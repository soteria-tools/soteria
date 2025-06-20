int main()
{
  int i = 1;
  int j = 0;
  while (i < 10)
  {
    i++;

    switch (i)
    {
    case 0:
      return -1;
    case 1:
      return -1;
    case 2:
      // First iteration: i=2, j=0;
      // Third iteration: i=2, j=2;
      if (j == 0)
      {
        j++;
        break;
      }
    case 3:
      // Second iteration i=3, j=1;
      switch (j)
      {
      case 0:
        return -1;
      case 1:
        i = 1;
        j++;
        break;
      default:
        i = 9;
        break;
      }
      break;
    case 4:
      return -1;
    default:
      // Fourth iteration: i=1000
      return 42;
    }
  }
  return -1;
}
