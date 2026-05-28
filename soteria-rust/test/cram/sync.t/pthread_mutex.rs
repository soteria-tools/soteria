// Exercises pthread synchronization primitives via direct extern "C" declarations.
// pthread_mutexattr_t / pthread_mutex_t / pthread_cond_t / pthread_rwlock_t are
// opaque in libc; we reserve enough space (64 bytes covers macOS and Linux).

#[repr(C, align(8))]
struct PthreadMutexattrT {
    _data: [u8; 64],
}

#[repr(C, align(8))]
struct PthreadMutexT {
    _data: [u8; 64],
}

// Linux glibc values; see soteria-rust/lib/builtins/extern/pthread.ml
const PTHREAD_MUTEX_NORMAL: i32 = 0;
const PTHREAD_MUTEX_RECURSIVE: i32 = 1;
const PTHREAD_MUTEX_ERRORCHECK: i32 = 2;

unsafe extern "C" {
    fn pthread_mutexattr_init(attr: *mut PthreadMutexattrT) -> i32;
    fn pthread_mutexattr_settype(attr: *mut PthreadMutexattrT, kind: i32) -> i32;
    fn pthread_mutexattr_destroy(attr: *mut PthreadMutexattrT) -> i32;
    fn pthread_mutex_init(mutex: *mut PthreadMutexT, attr: *const PthreadMutexattrT) -> i32;
    fn pthread_mutex_lock(mutex: *mut PthreadMutexT) -> i32;
    fn pthread_mutex_trylock(mutex: *mut PthreadMutexT) -> i32;
    fn pthread_mutex_unlock(mutex: *mut PthreadMutexT) -> i32;
    fn pthread_mutex_destroy(mutex: *mut PthreadMutexT) -> i32;
}

#[soteria::test]
fn mutexattr_init_succeeds() {
    let mut attr = PthreadMutexattrT { _data: [0; 64] };
    let ret = unsafe { pthread_mutexattr_init(&mut attr) };
    assert_eq!(ret, 0);
    unsafe { pthread_mutexattr_destroy(&mut attr) };
}

#[soteria::test]
fn mutex_lock_unlock() {
    let mut mutex = PthreadMutexT { _data: [0; 64] };
    let init = unsafe { pthread_mutex_init(&mut mutex, std::ptr::null()) };
    assert_eq!(init, 0);

    let l = unsafe { pthread_mutex_lock(&mut mutex) };
    assert_eq!(l, 0);

    let u = unsafe { pthread_mutex_unlock(&mut mutex) };
    assert_eq!(u, 0);

    unsafe { pthread_mutex_destroy(&mut mutex) };
}

/// Locking a default (Normal) mutex that is already held should be flagged as
/// a deadlock since we explore a single thread.
#[soteria::test]
fn mutex_double_lock_default_deadlocks() {
    let mut mutex = PthreadMutexT { _data: [0; 64] };
    unsafe { pthread_mutex_init(&mut mutex, std::ptr::null()) };
    unsafe { pthread_mutex_lock(&mut mutex) };
    let _ = unsafe { pthread_mutex_lock(&mut mutex) };
}

/// An ErrorCheck mutex returns EDEADLK (35) instead of deadlocking.
#[soteria::test]
fn mutex_double_lock_errorcheck_returns_edeadlk() {
    let mut attr = PthreadMutexattrT { _data: [0; 64] };
    unsafe { pthread_mutexattr_init(&mut attr) };
    unsafe { pthread_mutexattr_settype(&mut attr, PTHREAD_MUTEX_ERRORCHECK) };

    let mut mutex = PthreadMutexT { _data: [0; 64] };
    unsafe { pthread_mutex_init(&mut mutex, &attr) };
    unsafe { pthread_mutexattr_destroy(&mut attr) };

    unsafe { pthread_mutex_lock(&mut mutex) };
    let r = unsafe { pthread_mutex_lock(&mut mutex) };
    assert_eq!(r, 35);

    unsafe { pthread_mutex_unlock(&mut mutex) };
    unsafe { pthread_mutex_destroy(&mut mutex) };
}

/// A recursive mutex can be locked multiple times by the same thread.
#[soteria::test]
fn mutex_recursive_relock_succeeds() {
    let mut attr = PthreadMutexattrT { _data: [0; 64] };
    unsafe { pthread_mutexattr_init(&mut attr) };
    unsafe { pthread_mutexattr_settype(&mut attr, PTHREAD_MUTEX_RECURSIVE) };

    let mut mutex = PthreadMutexT { _data: [0; 64] };
    unsafe { pthread_mutex_init(&mut mutex, &attr) };
    unsafe { pthread_mutexattr_destroy(&mut attr) };

    assert_eq!(unsafe { pthread_mutex_lock(&mut mutex) }, 0);
    assert_eq!(unsafe { pthread_mutex_lock(&mut mutex) }, 0);
    assert_eq!(unsafe { pthread_mutex_unlock(&mut mutex) }, 0);
    assert_eq!(unsafe { pthread_mutex_unlock(&mut mutex) }, 0);

    unsafe { pthread_mutex_destroy(&mut mutex) };
}

/// trylock on a held non-recursive mutex returns EBUSY.
#[soteria::test]
fn mutex_trylock_busy() {
    let mut mutex = PthreadMutexT { _data: [0; 64] };
    unsafe { pthread_mutex_init(&mut mutex, std::ptr::null()) };

    assert_eq!(unsafe { pthread_mutex_trylock(&mut mutex) }, 0);
    assert_eq!(unsafe { pthread_mutex_trylock(&mut mutex) }, 16);

    unsafe { pthread_mutex_unlock(&mut mutex) };
    unsafe { pthread_mutex_destroy(&mut mutex) };
}

/// Destroying a still-locked mutex is flagged.
#[soteria::test]
fn mutex_destroy_locked_errors() {
    let mut mutex = PthreadMutexT { _data: [0; 64] };
    unsafe { pthread_mutex_init(&mut mutex, std::ptr::null()) };
    unsafe { pthread_mutex_lock(&mut mutex) };
    unsafe { pthread_mutex_destroy(&mut mutex) };
}
