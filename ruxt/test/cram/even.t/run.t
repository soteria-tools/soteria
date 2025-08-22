Test RUXt in even example
  $ ruxt unsafe_public.rs --clean --only-public
  Found type unsoundness!
  [1]

  $ ruxt safe_private.rs --clean
  No type unsoundness found!

  $ ruxt safe_public.rs --clean
  Found type unsoundness!
  [1]

  $ ruxt safe_public.rs --clean --only-public
  No type unsoundness found!
