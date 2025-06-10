  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___495, decl);
    __soteria_nondet__ -> (__soteria_nondet___493, decl);
    __stderr -> (__stderr_505, decl);
    __stdin -> (__stdin_506, decl);
    __stdout -> (__stdout_504, decl);
    add -> (add_488, def);
    clearerr -> (clearerr_631, decl);
    fclose -> (fclose_516, decl);
    fdopen -> (fdopen_638, decl);
    feof -> (feof_633, decl);
    ferror -> (ferror_635, decl);
    fflush -> (fflush_518, decl);
    fgetc -> (fgetc_582, decl);
    fgetpos -> (fgetpos_618, decl);
    fgets -> (fgets_586, decl);
    fileno -> (fileno_639, decl);
    fopen -> (fopen_521, decl);
    fprintf -> (fprintf_536, decl);
    fputc -> (fputc_589, decl);
    fputs -> (fputs_592, decl);
    fread -> (fread_610, decl);
    freopen -> (freopen_525, decl);
    fscanf -> (fscanf_539, decl);
    fseek -> (fseek_622, decl);
    fsetpos -> (fsetpos_625, decl);
    ftell -> (ftell_627, decl);
    fwrite -> (fwrite_615, decl);
    getc -> (getc_594, decl);
    getchar -> (getchar_595, decl);
    main -> (main_649, def);
    operate -> (operate_644, def);
    perror -> (perror_637, decl);
    printf -> (printf_541, decl);
    putc -> (putc_598, decl);
    putchar -> (putchar_600, decl);
    puts -> (puts_602, decl);
    remove -> (remove_508, decl);
    rename -> (rename_511, decl);
    rewind -> (rewind_629, decl);
    scanf -> (scanf_543, decl);
    setbuf -> (setbuf_528, decl);
    setvbuf -> (setvbuf_533, decl);
    snprintf -> (snprintf_547, decl);
    sprintf -> (sprintf_550, decl);
    sscanf -> (sscanf_553, decl);
    tmpfile -> (tmpfile_512, decl);
    tmpnam -> (tmpnam_514, decl);
    ungetc -> (ungetc_605, decl);
    vfprintf -> (vfprintf_557, decl);
    vfscanf -> (vfscanf_561, decl);
    vprintf -> (vprintf_564, decl);
    vscanf -> (vscanf_567, decl);
    vsnprintf -> (vsnprintf_572, decl);
    vsprintf -> (vsprintf_576, decl);
    vsscanf -> (vsscanf_580, decl)
  
  Declarations:
    __soteria_nondet___483 -> function;
    __assert___485 -> function;
    add_488 -> function;
    __soteria_nondet___493 -> function;
    __assert___495 -> function;
    add_498 -> function;
    __stdout_504 -> object;
    __stderr_505 -> object;
    __stdin_506 -> object;
    remove_508 -> function;
    rename_511 -> function;
    tmpfile_512 -> function;
    tmpnam_514 -> function;
    fclose_516 -> function;
    fflush_518 -> function;
    fopen_521 -> function;
    freopen_525 -> function;
    setbuf_528 -> function;
    setvbuf_533 -> function;
    fprintf_536 -> function;
    fscanf_539 -> function;
    printf_541 -> function;
    scanf_543 -> function;
    snprintf_547 -> function;
    sprintf_550 -> function;
    sscanf_553 -> function;
    vfprintf_557 -> function;
    vfscanf_561 -> function;
    vprintf_564 -> function;
    vscanf_567 -> function;
    vsnprintf_572 -> function;
    vsprintf_576 -> function;
    vsscanf_580 -> function;
    fgetc_582 -> function;
    fgets_586 -> function;
    fputc_589 -> function;
    fputs_592 -> function;
    getc_594 -> function;
    getchar_595 -> function;
    putc_598 -> function;
    putchar_600 -> function;
    puts_602 -> function;
    ungetc_605 -> function;
    fread_610 -> function;
    fwrite_615 -> function;
    fgetpos_618 -> function;
    fseek_622 -> function;
    fsetpos_625 -> function;
    ftell_627 -> function;
    rewind_629 -> function;
    clearerr_631 -> function;
    feof_633 -> function;
    ferror_635 -> function;
    perror_637 -> function;
    fdopen_638 -> function;
    fileno_639 -> function;
    operate_644 -> function;
    main_649 -> function
  
  Object definitions:
    
  
  Function definitions:
    add_488
    operate_644
    main_649
  
   Symmap:
    add_498 -> add_488;
    operate_644 -> operate_644;
    main_649 -> main_649;
    __soteria_nondet___483 -> __soteria_nondet___493;
    __assert___485 -> __assert___495;
    add_488 -> add_488
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare add WITH PROTO as function (signed int, signed int) returning signed int
  signed int add(signed int x, signed int y)
  {
    return rvalue(x) + rvalue(y);
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
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

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
