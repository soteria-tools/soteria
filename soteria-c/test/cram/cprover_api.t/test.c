void __CPROVER_assert(int condition, const char *message);

int main()
{
  __CPROVER_assert(0, "");
}