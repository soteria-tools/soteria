void *malloc(__cerbty_size_t size);

int main()
{
  int *x = malloc(sizeof(int));
  if (!x)
    return 1;
  *x = 12;
  *x = *&*x;
  return 0;
}
