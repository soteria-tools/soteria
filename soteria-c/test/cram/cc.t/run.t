  $ soteria-c exec-main array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 24}; (Bound 24)];
                info = (Some array_add.c:120:15-36) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:123:24-27 (cursor: 123:24)) });
             (V|3|,
              { node =
                [TypedVal {offset = 0; ty = size_t; v = 10};
                 TypedVal {offset = 8; ty = size_t; v = 16};
                 TypedVal {offset = 16; ty = void**; v = &(V|5|, 0)};
                 (Bound 24)];
                info = (Some array_add.c:35:15-36) });
             (V|4|, { node = Freed; info = (Some array_add.c:37:17-42) });
             (V|5|,
              { node =
                [TypedVal {offset = 0; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 8; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 16; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 24; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 32; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 40; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 48; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 56; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 64; ty = void*; v = &(0, 0)};
                 TypedVal {offset = 72; ty = void*; v = &(0, 0)};
                 Uninit {offset = 80; len = 48}; (Bound 128)];
                info = (Some array_add.c:62:23-60) })];
            globs = [] });
     Error: Buffer overflow or underflow with trace
            [• Called from here: array_add.c:127:7-31;
             • Triggering write: array_add.c:79:3-33 (cursor: 79:24)];
     Ok: (1,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 24}; (Bound 24)];
                info = (Some array_add.c:120:15-36) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:123:24-27 (cursor: 123:24)) });
             (V|3|, { node = Freed; info = (Some array_add.c:35:15-36) })];
            globs = [] });
     Ok: (1,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 24}; (Bound 24)];
                info = (Some array_add.c:120:15-36) });
             (V|2|,
              { node = Freed;
                info = (Some array_add.c:123:24-27 (cursor: 123:24)) })];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 163 statements
