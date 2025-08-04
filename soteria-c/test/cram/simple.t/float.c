void __assert__(int);

int main()
{
  float x = 1.0f;
  float y = 2.0f;

  __assert__(y > x);
}