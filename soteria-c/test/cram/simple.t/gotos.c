int main()
{
  int i = __soteria___nondet_int();
  if (i > 0)
  {
    goto lab1;
    return -1;
  lab3:
    return 1042;
  }
  else
  {
    return 1043;
  lab1:
    i = 0;
    goto lab2;
  }
  while (1)
  {
    i = 1000;
  lab2:
    i++;
    if (i > 10)
    {
      if (i < 500)
      {
        return -1;
      }
      goto lab3;
    }
  }
  return -1;
}