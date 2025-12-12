void *malloc(__cerbty_size_t);

int main()
{
  int *p = (int *)malloc(sizeof(int));
  // We don't check for null deref, but that UB should be ignored with --ignore-ub.
  *p = 12;
  __soteria___assert(*p == 12);
}