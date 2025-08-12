// Left declared but undefined on purpose.
int nondet_int();

int main()
{
  int v = nondet_int();
  if (v < 0)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}