int x = 0;

void eval(void)
{
  while (1)
  {
    x = 1;
    break;
  }
  return;
}

int main()
{

  while (1)
  {
    eval();
    __soteria___assert(x == 0);
  }

  __soteria___assert(x == 0);

  return 0;
}