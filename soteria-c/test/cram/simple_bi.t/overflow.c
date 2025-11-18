int add(int a, int b)
{
  return a + b;
}

int add_ovf_manifest(int a)
{
  int b = 2147483647;
  int c = b + 1;
  return add(a, 1);
}
