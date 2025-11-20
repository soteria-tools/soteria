void __CPROVER_assert(int condition, const char *message);
void __CPROVER_assume(int condition);

int main()
{
  __CPROVER_assume(1);
  __CPROVER_assert(0, "");
}