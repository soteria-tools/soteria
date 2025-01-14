void *malloc(__cerbty_size_t);

int load(int *x)
{
  return *x;
}

int test()
{
  int *x = (int *)malloc(sizeof(int));
  // *x = 12;
  load(x);
}