void *malloc(__cerbty_size_t size);
void *memcpy(void *dest, const void *src, __cerbty_size_t n);

int main()
{

  int *x = malloc(2 * sizeof(int));
  if (!x)
    return 0;
  x[0] = 0;
  x[1] = 1;
  int *y = malloc(2 * sizeof(int));
  if (!y)
    return 0;
  memcpy(y, x, 2 * sizeof(int));
  return ((x[0] == y[0]) && (x[1] == y[1]));
}