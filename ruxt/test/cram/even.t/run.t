Test RUXt in even example
  $ ruxt safe_even.rs --clean
  No type unsoundness found!

  $ ruxt unsafe_even.rs --clean
  Found type unsoundness!
  [1]
