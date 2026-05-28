Test pthread synchronization primitives.
  $ soteria-rust exec pthread_mutex.rs
  Compiling... done in <time>
  => Running pthread_mutex::mutexattr_init_succeeds...
  note: pthread_mutex::mutexattr_init_succeeds: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pthread_mutex::mutex_lock_unlock...
  note: pthread_mutex::mutex_lock_unlock: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pthread_mutex::mutex_double_lock_default_deadlocks...
  error: pthread_mutex::mutex_double_lock_default_deadlocks: found issues in <time>, errors in 1 branch (out of 1)
  bug: UB in std: pthread_mutex_lock: deadlock on already-locked mutex in pthread_mutex::mutex_double_lock_default_deadlocks
      --> $TESTCASE_ROOT/pthread_mutex.rs:61:22
   57 |  fn mutex_double_lock_default_deadlocks() {
      |  ---------------------------------------- 1: Entry point
      .  
   61 |      let _ = unsafe { pthread_mutex_lock(&mut mutex) };
      |                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |                       |
      |                       Triggering operation
      |                       2: Call trace
  PC 1: empty
  
  => Running pthread_mutex::mutex_double_lock_errorcheck_returns_edeadlk...
  note: pthread_mutex::mutex_double_lock_errorcheck_returns_edeadlk: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pthread_mutex::mutex_recursive_relock_succeeds...
  note: pthread_mutex::mutex_recursive_relock_succeeds: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pthread_mutex::mutex_trylock_busy...
  note: pthread_mutex::mutex_trylock_busy: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pthread_mutex::mutex_destroy_locked_errors...
  error: pthread_mutex::mutex_destroy_locked_errors: found issues in <time>, errors in 1 branch (out of 1)
  bug: UB in std: pthread_mutex_destroy: destroying a locked mutex in pthread_mutex::mutex_destroy_locked_errors
      --> $TESTCASE_ROOT/pthread_mutex.rs:121:14
  117 |  fn mutex_destroy_locked_errors() {
      |  -------------------------------- 1: Entry point
      .  
  121 |      unsafe { pthread_mutex_destroy(&mut mutex) };
      |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |               |
      |               Triggering operation
      |               2: Call trace
  PC 1: empty
  
  [1]
