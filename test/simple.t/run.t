Just reading an empty file
  $ bfa-c empty.c -q
  Symex terminated with the following outcomes: [Ok (0, ())]

  $ bfa-c conc.c -q
  Symex terminated with the following outcomes: [Ok (2, ())]

  $ bfa-c sym.c -q
  Symex terminated with the following outcomes: [Ok (1, ()); Ok (2, ())]
