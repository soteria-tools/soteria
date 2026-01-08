Simple polymorphic function with no polymorphic values
  $ soteria-rust rustc trivial.rs --frontend charon --poly --clean --no-timing
  Compiling... done in <time>
  note: trivial::trivial: done in <time>, ran 1 branch
  PC 1: (0x0000000000000000 <=s (V|1| + 0x0000000000000008)) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((V|1| +ck 0x0000000000000010) -ck extend[u61](extract[0-2](V|1|))))) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000004 <=s ((V|1| +ck 0x0000000000000010) -ck extend[u61](extract[0-2](V|1|))))) /\
        ((extract[0-2](V|1|) == 0b000) || (((V|1| +ck 0x0000000000000010) -ck extend[u61](extract[0-2](V|1|))) != 0x0000000000000004)) /\
        (V|1| <=u 0x00000000000003ff)
  
Ensure generic args are passed through function calls and not lost
  $ soteria-rust rustc subst_generics.rs --frontend charon --poly --clean --no-timing
  Compiling... done in <time>
  note: subst_generics::wrap_stuff: done in <time>, ran 1 branch
  PC 1: empty
  
