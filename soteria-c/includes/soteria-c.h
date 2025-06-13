int __soteria_nondet__();

#define __attribute__(X)

// Dealing with Apple extensions
// Uncomment this or pass "-fno-blocks" to clang's cpp
#undef __BLOCKS__
#define _Nonnull
#define _Nullable

// Non standard 128 bits type
#define __uint128_t __cerbty_uint128_t

// Mapping clang builtin idents for stdarg.h to Cerberus' counterparts
#define __builtin_va_start __cerb_va_start
#define __builtin_va_end __cerb_va_end
#define __builtin_va_arg(ap, type) __cerb_va_arg(ap, type)
// TODO(HACK): this is a hack which is sufficient if we only care about
// the frontend
typedef void *__builtin_va_list;

// TODO(HACK): this disables the detection of extensions so that they are not
// used when preprocessing Apple's libc (NOTE: this was at least needed to
// disable fixed enums which was a C++ extensions but has now been added in
// C23).
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wbuiltin-macro-redefined"
#undef __has_extension
#pragma clang diagnostic pop
#define __has_extension(x) 0

// TODO(HACK): Cerberus is missing support for this.
#define __func__ "__func__"

// Mapping clang's builtin offsetof back to Cerberus' counterpart
#define __builtin_offsetof offsetof

// TODO(HACK): Cerberus does not support this builtin, so implement it as
// always returning false
#define __builtin_constant_p(X) (0)

// Declaring GNU C Arithmetic with overflow checking builtins as needed
// see https://gcc.gnu.org/onlinedocs/gcc/Integer-Overflow-Builtins.html
_Bool __builtin_sadd_overflow(int a, int b, int *res);
_Bool __builtin_umul_overflow(unsigned int a, unsigned int b, unsigned int *res);
_Bool __builtin_umull_overflow(unsigned long a, unsigned long b, unsigned long *res);
_Bool __builtin_uaddl_overflow(unsigned long int a, unsigned long int b, unsigned long int *res);
_Bool __builtin_ssub_overflow(int a, int b, int *res);

// Adding more esoteric builtins
void *__builtin___memset_chk(void *dest, int c, __cerbty_size_t len, __cerbty_size_t destlen);
void *__builtin___memcpy_chk(void *dest, const void *src, __cerbty_size_t n, __cerbty_size_t os);
void *__builtin___memmove_chk(void *dest, const void *src, __cerbty_size_t n, __cerbty_size_t os);
char *__builtin___strcpy_chk(char *dest, const char *src, __cerbty_size_t os);
int __builtin___snprintf_chk(char *s, __cerbty_size_t maxlen, int flag, __cerbty_size_t os, ...);
char *__builtin___strncpy_chk(char *dest, const char *src, __cerbty_size_t n, __cerbty_size_t os);
__cerbty_size_t __builtin_object_size(const void *ptr, int type);

__cerbty_uint32_t __builtin_bswap32(__cerbty_uint32_t x);
__cerbty_uint64_t __builtin_bswap64(__cerbty_uint64_t x);