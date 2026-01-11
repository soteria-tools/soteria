Simple polymorphic function with no polymorphic values
  $ soteria-rust rustc trivial.rs --frontend charon --poly
  Compiling... done in <time>
  note: trivial::trivial: done in <time>, ran 2 branches
  PC 1: (0x0000000000000000 <=s (0x0000000000000008 + V|1|)) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000000 != ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) ? (0x0000000000000000 == V|1|) : (0x0000000000000008 == ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (V|1| <=u 0x00000000000003ff)
  PC 2: (0x0000000000000000 <=s (0x0000000000000008 + V|1|)) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000000 != ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=u ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        !(((0b000 == extract[0-2](V|1|)) ? (0x0000000000000000 == V|1|) : (0x0000000000000008 == ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (V|1| <=u 0x00000000000003ff)
  
Ensure generic args are passed through function calls and not lost
  $ soteria-rust rustc subst_generics.rs --frontend charon --poly
  Compiling... done in <time>
  note: subst_generics::wrap_stuff: done in <time>, ran 1 branch
  PC 1: empty
  
Try creating a generic vec
  $ soteria-rust rustc vec.rs --frontend charon --poly
  Compiling... done in <time>
  note: vec::with_vec: done in <time>, ran 2 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  PC 2: (V|2| <u (0x7fffffffffffffff - (0x000000000000000a *ck V|1|))) /\
        (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x00000000000003ff) /\
        (0x0000000000000001 <=u V|2|)
  
  error: vec::with_vec_wrong: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: assertion failed: my_vec.capacity() == 10 in vec::with_vec_wrong
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:9
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │            │
      │            Triggering memory operation
      │            2: Call trace
      ┌─ $TESTCASE_ROOT/vec.rs:14:1
   13 │    #[rusteria::test]
   14 │ ╭  fn with_vec_wrong<T>() {
   15 │ │      let my_vec: Vec<T> = Vec::with_capacity(10);
   16 │ │      assert!(my_vec.capacity() == 10);
   17 │ │  }
      │ ╰──' 1: Entry point
   18 │    
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  
  [1]
