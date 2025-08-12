void *malloc(__cerbty_size_t);

int main()
{
  int *x = (int *)malloc(sizeof(int));
  if (!x)
    return 1;
  return 0;
}