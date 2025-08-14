  $ soteria-c exec array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [MemVal {offset = 0; len = 24; v = SUninit}; Bound(24)];
                info = (Some array_add.c:131:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:135:26-29 (cursor: 135:26)) });
             (V|3|,
              { node =
                [MemVal {offset = 0; len = 8; v = 10 : size_t};
                 MemVal {offset = 8; len = 8; v = 16 : size_t};
                 MemVal {offset = 16; len = 8; v = &(V|5|, 0) : void**};
                 Bound(24)];
                info = (Some array_add.c:34:17-38) });
             (V|4|, { node = Freed; info = (Some array_add.c:37:19-45) });
             (V|5|,
              { node =
                [MemVal {offset = 0; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 8; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 16; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 24; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 32; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 40; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 48; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 56; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 64; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 72; len = 8; v = &(0, 0) : void*};
                 MemVal {offset = 80; len = 48; v = SUninit}; Bound(128)];
                info = (Some array_add.c:64:23-60) })];
            globs = [] });
     Error: Buffer overflow or underflow with trace
            [• Called from here: array_add.c:141:9-33;
             • Triggering write: array_add.c:84:5-35 (cursor: 84:26)];
     Ok: (1,
          { heap =
            [(V|1|,
              { node = [MemVal {offset = 0; len = 24; v = SUninit}; Bound(24)];
                info = (Some array_add.c:131:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:135:26-29 (cursor: 135:26)) });
             (V|3|, { node = Freed; info = (Some array_add.c:34:17-38) })];
            globs = [] });
     Ok: (1,
          { heap =
            [(V|1|,
              { node = [MemVal {offset = 0; len = 24; v = SUninit}; Bound(24)];
                info = (Some array_add.c:131:17-38) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:135:26-29 (cursor: 135:26)) })];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 163 statements
