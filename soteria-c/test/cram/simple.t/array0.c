int main()
{
  char str_a[] = {'a', 'b', '\0'};
  char str_b[] = {'a', 'b', '\0'};
  __soteria___assert(str_a[0] == str_b[0]);
  __soteria___assert(str_a[1] == str_b[1]);
  return 0;
}