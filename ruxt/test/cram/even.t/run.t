Test RUXt in even example
  $ ruxt unsafe_public.rs --clean --no-timing --only-public
  Compiling... done in <time>
  Found type unsoundness!
  [1]

  $ ruxt safe_private.rs --clean --no-timing
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt safe_public.rs --clean --no-timing
  Compiling... done in <time>
  Found type unsoundness!
  [1]

  $ ruxt safe_public.rs --clean --no-timing --only-public
  Compiling... done in <time>
  No type unsoundness found!
