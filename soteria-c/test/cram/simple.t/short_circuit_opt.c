// Test that we do optimise short circuit evaluation if the LHS is not side-effectful.

int main()
{
  int x = __soteria_nondet__();
  int y = __soteria_nondet__();
  // If this returns a single branch, optimisation is working.
  return x && y;
}