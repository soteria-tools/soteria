void *malloc(__cerbty_size_t size);

int main()
{
  int *x = malloc(1024);
  *x = 12;

  return 0;
}