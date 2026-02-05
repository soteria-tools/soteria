  $ soteria-c exec array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  error: Buffer overflow or underflow in main
      ┌─ array_add.c:104:5
  104 │      ar->buffer[ar->size] = element;
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Invalid memory write
  ... ·  
  157 │          array_add(v1, NULL);
      │          -------------------- 1: Called from here
  Executed 163 statements
  Verification Failure!
  [13]
