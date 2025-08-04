void __assert__(int);
void *calloc(__cerbty_size_t, __cerbty_size_t);

float f;

int main()
{
  float x = 1.0f;
  float y = 2.0f;
  float *f = (float *)calloc(1, sizeof(float));

  __assert__(y > x);
  __assert__(*f == 0.0f);
}