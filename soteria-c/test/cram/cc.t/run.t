  $ soteria-c exec array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000018;
                   v = SUninit};
                 Bound(0x0000000000000018)];
                info = (Some array_add.c:151:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:155:26-29 (cursor: 155:26)) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = 0x000000000000000a : size_t};
                 MemVal {offset = 0x0000000000000008; len = 0x0000000000000008;
                   v = 0x0000000000000010 : size_t};
                 MemVal {offset = 0x0000000000000010; len = 0x0000000000000008;
                   v = &(V|5|, 0x0000000000000000) : void**};
                 Bound(0x0000000000000018)];
                info = (Some array_add.c:54:17-38) });
             (V|4|, { node = Freed; info = (Some array_add.c:57:19-45) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000008; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000010; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000018; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000020; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000028; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000030; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000038; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000040; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000048; len = 0x0000000000000008;
                   v = &(0x0000000000000000, 0x0000000000000000) : void*};
                 MemVal {offset = 0x0000000000000050; len = 0x0000000000000030;
                   v = SUninit};
                 Bound(0x0000000000000080)];
                info = (Some array_add.c:84:23-60) })];
            globs = [] });
     Error: Buffer overflow or underflow with trace
            [• Called from here: array_add.c:161:9-33;
             • Triggering write: array_add.c:104:5-35 (cursor: 104:26)];
     Ok: (0x00000001,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000018;
                   v = SUninit};
                 Bound(0x0000000000000018)];
                info = (Some array_add.c:151:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:155:26-29 (cursor: 155:26)) });
             (V|3|, { node = Freed; info = (Some array_add.c:54:17-38) })];
            globs = [] });
     Ok: (0x00000001,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000018;
                   v = SUninit};
                 Bound(0x0000000000000018)];
                info = (Some array_add.c:151:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:155:26-29 (cursor: 155:26)) })];
            globs = [] });
     Ok: (0x00000001, { heap = []; globs = [] })]
  Executed 163 statements
