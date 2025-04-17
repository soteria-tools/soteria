void *malloc(__cerbty_size_t);
void free(void *);

int load(int *x)
{
  return *x;
}

int test_np_uninit()
{
  int *x = (int *)malloc(sizeof(int));
  load(x);
  return 0;
}

int test_uninit()
{
  int *x = (int *)malloc(sizeof(int));
  if (!x)
    return 1;
  load(x);
  free(x);
  return 0;
}

int test_leak()
{
  int *x = (int *)malloc(sizeof(int));
  if (!x)
    return 1;
  *x = 12;
  load(x);
  return 0;
}

int test_ok()
{

  int *x = (int *)malloc(sizeof(int));
  if (!x)
    return 1;
  *x = 12;
  load(x);
  free(x);
  return 0;
}

int test_np()
{
  int *x = (int *)malloc(sizeof(int));
  *x = 12;
  load(x);
  free(x);
  return 0;
}