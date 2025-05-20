  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___493, decl);
    __stderr -> (__stderr_503, decl);
    __stdin -> (__stdin_504, decl);
    __stdout -> (__stdout_502, decl);
    add -> (add_487, def);
    clearerr -> (clearerr_629, decl);
    fclose -> (fclose_514, decl);
    fdopen -> (fdopen_636, decl);
    feof -> (feof_631, decl);
    ferror -> (ferror_633, decl);
    fflush -> (fflush_516, decl);
    fgetc -> (fgetc_580, decl);
    fgetpos -> (fgetpos_616, decl);
    fgets -> (fgets_584, decl);
    fileno -> (fileno_637, decl);
    fopen -> (fopen_519, decl);
    fprintf -> (fprintf_534, decl);
    fputc -> (fputc_587, decl);
    fputs -> (fputs_590, decl);
    fread -> (fread_608, decl);
    freopen -> (freopen_523, decl);
    fscanf -> (fscanf_537, decl);
    fseek -> (fseek_620, decl);
    fsetpos -> (fsetpos_623, decl);
    ftell -> (ftell_625, decl);
    fwrite -> (fwrite_613, decl);
    getc -> (getc_592, decl);
    getchar -> (getchar_593, decl);
    main -> (main_647, def);
    operate -> (operate_642, def);
    perror -> (perror_635, decl);
    printf -> (printf_539, decl);
    putc -> (putc_596, decl);
    putchar -> (putchar_598, decl);
    puts -> (puts_600, decl);
    remove -> (remove_506, decl);
    rename -> (rename_509, decl);
    rewind -> (rewind_627, decl);
    scanf -> (scanf_541, decl);
    setbuf -> (setbuf_526, decl);
    setvbuf -> (setvbuf_531, decl);
    snprintf -> (snprintf_545, decl);
    sprintf -> (sprintf_548, decl);
    sscanf -> (sscanf_551, decl);
    tmpfile -> (tmpfile_510, decl);
    tmpnam -> (tmpnam_512, decl);
    ungetc -> (ungetc_603, decl);
    vfprintf -> (vfprintf_555, decl);
    vfscanf -> (vfscanf_559, decl);
    vprintf -> (vprintf_562, decl);
    vscanf -> (vscanf_565, decl);
    vsnprintf -> (vsnprintf_570, decl);
    vsprintf -> (vsprintf_574, decl);
    vsscanf -> (vsscanf_578, decl)
  
  Declarations:
    __assert___484 -> function;
    add_487 -> function;
    __assert___493 -> function;
    add_496 -> function;
    __stdout_502 -> object;
    __stderr_503 -> object;
    __stdin_504 -> object;
    remove_506 -> function;
    rename_509 -> function;
    tmpfile_510 -> function;
    tmpnam_512 -> function;
    fclose_514 -> function;
    fflush_516 -> function;
    fopen_519 -> function;
    freopen_523 -> function;
    setbuf_526 -> function;
    setvbuf_531 -> function;
    fprintf_534 -> function;
    fscanf_537 -> function;
    printf_539 -> function;
    scanf_541 -> function;
    snprintf_545 -> function;
    sprintf_548 -> function;
    sscanf_551 -> function;
    vfprintf_555 -> function;
    vfscanf_559 -> function;
    vprintf_562 -> function;
    vscanf_565 -> function;
    vsnprintf_570 -> function;
    vsprintf_574 -> function;
    vsscanf_578 -> function;
    fgetc_580 -> function;
    fgets_584 -> function;
    fputc_587 -> function;
    fputs_590 -> function;
    getc_592 -> function;
    getchar_593 -> function;
    putc_596 -> function;
    putchar_598 -> function;
    puts_600 -> function;
    ungetc_603 -> function;
    fread_608 -> function;
    fwrite_613 -> function;
    fgetpos_616 -> function;
    fseek_620 -> function;
    fsetpos_623 -> function;
    ftell_625 -> function;
    rewind_627 -> function;
    clearerr_629 -> function;
    feof_631 -> function;
    ferror_633 -> function;
    perror_635 -> function;
    fdopen_636 -> function;
    fileno_637 -> function;
    operate_642 -> function;
    main_647 -> function
  
  Object definitions:
    
  
  Function definitions:
    add_487
    operate_642
    main_647
  
   Symmap:
    add_496 -> add_487;
    operate_642 -> operate_642;
    main_647 -> main_647;
    __assert___484 -> __assert___493;
    add_487 -> add_487
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare add WITH PROTO as function (signed int, signed int) returning signed int
  signed int add(signed int x, signed int y)
  {
    return rvalue(x) + rvalue(y);
  }
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare add WITH PROTO as function (signed int, signed int) returning signed int
  signed int add(signed int, signed int);
  
  // declare __stdout as {const} pointer to struct _IO_FILE
  struct _IO_FILE*const  __stdout;
  
  // declare __stderr as {const} pointer to struct _IO_FILE
  struct _IO_FILE*const  __stderr;
  
  // declare __stdin as {const} pointer to struct _IO_FILE
  struct _IO_FILE*const  __stdin;
  
  // declare remove WITH PROTO as function (pointer to {const} char) returning signed int
  signed int remove(const char*);
  
  // declare rename WITH PROTO as function (pointer to {const} char, pointer to {const} char) returning signed int
  signed int rename(const char*, const char*);
  
  // declare tmpfile WITH PROTO as function () returning pointer to struct _IO_FILE
  struct _IO_FILE* tmpfile();
  
  // declare tmpnam WITH PROTO as function (pointer to char) returning pointer to char
  char* tmpnam(char*);
  
  // declare fclose WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int fclose(struct _IO_FILE*);
  
  // declare fflush WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int fflush(struct _IO_FILE*);
  
  // declare fopen WITH PROTO as function ({restrict} pointer to {const} char, {restrict} pointer to {const} char) returning pointer to struct _IO_FILE
  struct _IO_FILE* fopen(const char*restrict , const char*restrict );
  
  // declare freopen WITH PROTO as function ({restrict} pointer to {const} char, {restrict} pointer to {const} char, {restrict} pointer to struct _IO_FILE) returning pointer to struct _IO_FILE
  struct _IO_FILE* freopen(const char*restrict , const char*restrict , struct _IO_FILE*restrict );
  
  // declare setbuf WITH PROTO as function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to char) returning void
  void setbuf(struct _IO_FILE*restrict , char*restrict );
  
  // declare setvbuf WITH PROTO as function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to char, signed int, size_t) returning signed int
  signed int setvbuf(struct _IO_FILE*restrict , char*restrict , signed int, size_t);
  
  // declare fprintf WITH PROTO as variadic function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to {const} char) returning signed int
  signed int fprintf(struct _IO_FILE*restrict , const char*restrict , ...);
  
  // declare fscanf WITH PROTO as variadic function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to {const} char) returning signed int
  signed int fscanf(struct _IO_FILE*restrict , const char*restrict , ...);
  
  // declare printf WITH PROTO as variadic function ({restrict} pointer to {const} char) returning signed int
  signed int printf(const char*restrict , ...);
  
  // declare scanf WITH PROTO as variadic function ({restrict} pointer to {const} char) returning signed int
  signed int scanf(const char*restrict , ...);
  
  // declare snprintf WITH PROTO as variadic function ({restrict} pointer to char, size_t, {restrict} pointer to {const} char) returning signed int
  signed int snprintf(char*restrict , size_t, const char*restrict , ...);
  
  // declare sprintf WITH PROTO as variadic function ({restrict} pointer to char, {restrict} pointer to {const} char) returning signed int
  signed int sprintf(char*restrict , const char*restrict , ...);
  
  // declare sscanf WITH PROTO as variadic function ({restrict} pointer to {const} char, {restrict} pointer to {const} char) returning signed int
  signed int sscanf(const char*restrict , const char*restrict , ...);
  
  // declare vfprintf WITH PROTO as function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to {const} char, signed int) returning signed int
  signed int vfprintf(struct _IO_FILE*restrict , const char*restrict , signed int);
  
  // declare vfscanf WITH PROTO as function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to {const} char, signed int) returning signed int
  signed int vfscanf(struct _IO_FILE*restrict , const char*restrict , signed int);
  
  // declare vprintf WITH PROTO as function ({restrict} pointer to {const} char, signed int) returning signed int
  signed int vprintf(const char*restrict , signed int);
  
  // declare vscanf WITH PROTO as function ({restrict} pointer to {const} char, signed int) returning signed int
  signed int vscanf(const char*restrict , signed int);
  
  // declare vsnprintf WITH PROTO as function ({restrict} pointer to char, size_t, {restrict} pointer to {const} char, signed int) returning signed int
  signed int vsnprintf(char*restrict , size_t, const char*restrict , signed int);
  
  // declare vsprintf WITH PROTO as function ({restrict} pointer to char, {restrict} pointer to {const} char, signed int) returning signed int
  signed int vsprintf(char*restrict , const char*restrict , signed int);
  
  // declare vsscanf WITH PROTO as function ({restrict} pointer to {const} char, {restrict} pointer to {const} char, signed int) returning signed int
  signed int vsscanf(const char*restrict , const char*restrict , signed int);
  
  // declare fgetc WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int fgetc(struct _IO_FILE*);
  
  // declare fgets WITH PROTO as function ({restrict} pointer to char, signed int, {restrict} pointer to struct _IO_FILE) returning pointer to char
  char* fgets(char*restrict , signed int, struct _IO_FILE*restrict );
  
  // declare fputc WITH PROTO as function (signed int, pointer to struct _IO_FILE) returning signed int
  signed int fputc(signed int, struct _IO_FILE*);
  
  // declare fputs WITH PROTO as function ({restrict} pointer to {const} char, {restrict} pointer to struct _IO_FILE) returning signed int
  signed int fputs(const char*restrict , struct _IO_FILE*restrict );
  
  // declare getc WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int getc(struct _IO_FILE*);
  
  // declare getchar WITH PROTO as function () returning signed int
  signed int getchar();
  
  // declare putc WITH PROTO as function (signed int, pointer to struct _IO_FILE) returning signed int
  signed int putc(signed int, struct _IO_FILE*);
  
  // declare putchar WITH PROTO as function (signed int) returning signed int
  signed int putchar(signed int);
  
  // declare puts WITH PROTO as function (pointer to {const} char) returning signed int
  signed int puts(const char*);
  
  // declare ungetc WITH PROTO as function (signed int, pointer to struct _IO_FILE) returning signed int
  signed int ungetc(signed int, struct _IO_FILE*);
  
  // declare fread WITH PROTO as function ({restrict} pointer to void, size_t, size_t, {restrict} pointer to struct _IO_FILE) returning size_t
  size_t fread(void*restrict , size_t, size_t, struct _IO_FILE*restrict );
  
  // declare fwrite WITH PROTO as function ({restrict} pointer to {const} void, size_t, size_t, {restrict} pointer to struct _IO_FILE) returning size_t
  size_t fwrite(const void*restrict , size_t, size_t, struct _IO_FILE*restrict );
  
  // declare fgetpos WITH PROTO as function ({restrict} pointer to struct _IO_FILE, {restrict} pointer to signed long long) returning signed int
  signed int fgetpos(struct _IO_FILE*restrict , signed long long*restrict );
  
  // declare fseek WITH PROTO as function (pointer to struct _IO_FILE, signed long, signed int) returning signed int
  signed int fseek(struct _IO_FILE*, signed long, signed int);
  
  // declare fsetpos WITH PROTO as function (pointer to struct _IO_FILE, pointer to {const} signed long long) returning signed int
  signed int fsetpos(struct _IO_FILE*, const signed long long*);
  
  // declare ftell WITH PROTO as function (pointer to struct _IO_FILE) returning signed long
  signed long ftell(struct _IO_FILE*);
  
  // declare rewind WITH PROTO as function (pointer to struct _IO_FILE) returning void
  void rewind(struct _IO_FILE*);
  
  // declare clearerr WITH PROTO as function (pointer to struct _IO_FILE) returning void
  void clearerr(struct _IO_FILE*);
  
  // declare feof WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int feof(struct _IO_FILE*);
  
  // declare ferror WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int ferror(struct _IO_FILE*);
  
  // declare perror WITH PROTO as function (pointer to {const} char) returning void
  void perror(const char*);
  
  // declare fdopen WITH PROTO as function (signed int, pointer to {const} char) returning pointer to struct _IO_FILE
  struct _IO_FILE* fdopen(signed int, const char*);
  
  // declare fileno WITH PROTO as function (pointer to struct _IO_FILE) returning signed int
  signed int fileno(struct _IO_FILE*);
  
  // declare operate WITH PROTO as function (pointer to function (signed int, signed int) returning signed int, signed int, signed int) returning signed int
  signed int operate(signed int (* op) (signed int, signed int), signed int a, signed int b)
  {
    return rvalue(op)(rvalue(a), rvalue(b));
  }
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(operate)(function_decay(add), 5, 3);
    function_decay(__assert__)((rvalue(ret) == 8));
    return 0;
  }

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
             (V|4|, Freed); (V|5|, Freed)];
            globs = [] })]
  Executed 8 statements
