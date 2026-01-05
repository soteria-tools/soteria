Test RUXt in even example
  $ ruxt unsafe_public.rs --only-public
  Compiling... done in <time>
  Found type unsoundness!
  [1]

  $ ruxt safe_private.rs
  Compiling... done in <time>
  No type unsoundness found!

  $ ruxt safe_public.rs
  Compiling... done in <time>
  Found type unsoundness!
  [1]

  $ ruxt safe_public.rs --only-public
  Compiling... done in <time>
  No type unsoundness found!
