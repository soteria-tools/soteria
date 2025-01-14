void *malloc(__cerbty_size_t);
void free(void *);

int load(int *x)
{
  return *x;
}

int test_np_uninit()
{
  int *x = (int *)malloc(sizeof(int));
  // if (!x)
  //   return 1; // Uncomment to fix null pointer dereference
  // *x = 12; // Uncomment to fix uninitialized memory access
  load(x);
  // free(x); // Uncomment to fix memory leak
  return 0;
}
