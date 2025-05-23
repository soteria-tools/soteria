extern crate self as miristd;

#[repr(C)]
/// Layout of the return value of `miri_resolve_frame`,
/// with fields in the exact same order.
pub struct MiriFrame {
    // The size of the name of the function being executed, encoded in UTF-8
    pub _name_len: usize,
    // The size of filename of the function being executed, encoded in UTF-8
    pub _filename_len: usize,
    // The line number currently being executed in `filename`, starting from '1'.
    pub _lineno: u32,
    // The column number currently being executed in `filename`, starting from '1'.
    pub _colno: u32,
    // The function pointer to the function currently being executed.
    // This can be compared against function pointers obtained by
    // casting a function (e.g. `my_fn as *mut ()`)
    pub _fn_ptr: *mut (),
}

/// Miri-provided extern function to mark the block `ptr` points to as a "root"
/// for some static memory. This memory and everything reachable by it is not
/// considered leaking even if it still exists when the program terminates.
///
/// `ptr` has to point to the beginning of an allocated block.
pub fn miri_static_root(_ptr: *const u8) {
    unreachable!()
}

// Miri-provided extern function to get the amount of frames in the current backtrace.
// The `flags` argument must be `0`.
pub fn miri_backtrace_size(_flags: u64) -> usize {
    unreachable!()
}

/// Miri-provided extern function to obtain a backtrace of the current call stack.
/// This writes a slice of pointers into `buf` - each pointer is an opaque value
/// that is only useful when passed to `miri_resolve_frame`.
/// `buf` must have `miri_backtrace_size(0) * pointer_size` bytes of space.
/// The `flags` argument must be `1`.
pub fn miri_get_backtrace(_flags: u64, _buf: *mut *mut ()) {
    unreachable!()
}

/// Miri-provided extern function to resolve a frame pointer obtained
/// from `miri_get_backtrace`. The `flags` argument must be `1`.
///
/// This function can be called on any thread (not just the one which obtained `frame`).
pub fn miri_resolve_frame(_frame: *mut (), _flags: u64) -> MiriFrame {
    unreachable!()
}

/// Miri-provided extern function to get the name and filename of the frame provided by `miri_resolve_frame`.
/// `name_buf` and `filename_buf` should be allocated with the `name_len` and `filename_len` fields of `MiriFrame`.
/// The flags argument must be `0`.
pub fn miri_resolve_frame_names(
    _ptr: *mut (),
    _flags: u64,
    _name_buf: *mut u8,
    _filename_buf: *mut u8,
) {
    unreachable!()
}

/// Miri-provided extern function to begin unwinding with the given payload.
///
/// This is internal and unstable and should not be used; we give it here
/// just to be complete.
pub fn miri_start_unwind(_payload: *mut u8) -> ! {
    unreachable!()
}

/// Miri-provided extern function to get the internal unique identifier for the allocation that a pointer
/// points to. If this pointer is invalid (not pointing to an allocation), interpretation will abort.
///
/// This is only useful as an input to `miri_print_borrow_stacks`, and it is a separate call because
/// getting a pointer to an allocation at runtime can change the borrow stacks in the allocation.
/// This function should be considered unstable. It exists only to support `miri_print_borrow_state` and so
/// inherits all of its instability.
pub fn miri_get_alloc_id(_ptr: *const ()) -> u64 {
    unreachable!()
}

/// Miri-provided extern function to print (from the interpreter, not the program) the contents of all
/// borrows in an allocation.
///
/// If Stacked Borrows is running, this prints all the stacks. The leftmost tag is the bottom of the stack.
///
/// If Tree borrows is running, this prints on the left the permissions of each tag on each range,
/// an on the right the tree structure of the tags. If some tags were named via `miri_pointer_name`,
/// their names appear here.
///
/// If additionally `show_unnamed` is `false` then tags that did *not* receive a name will be hidden.
/// Ensure that either the important tags have been named, or `show_unnamed = true`.
/// _Note: as Stacked Borrows does not have tag names at all, `show_unnamed` is ignored and all tags are shown.
/// In general, unless you strongly want some tags to be hidden (as is the case in `tree-borrows` tests),
/// `show_unnamed = true` should be the default.
///
/// The format of what this emits is unstable and may change at any time. In particular, users should be
/// aware that Miri will periodically attempt to garbage collect the contents of all stacks. Callers of
/// this function may wish to pass `-Zmiri-provenance-gc=0` to disable the GC.
///
/// This function is extremely unstable. At any time the format of its output may change, its signature may
/// change, or it may be removed entirely.
pub fn miri_print_borrow_state(_alloc_id: u64, _show_unnamed: bool) {
    unreachable!()
}

/// Miri-provided extern function to associate a name to the nth parent of a tag.
/// Typically the name given would be the name of the program variable that holds the pointer.
/// Unreachable tags can still be named by using nonzero `nth_parent` and a child tag.
///
/// This function does nothing under Stacked Borrows, since Stacked Borrows's implementation
/// of `miri_print_borrow_state` does not show the names.
///
/// Under Tree Borrows, the names also appear in error messages.
pub fn miri_pointer_name(_ptr: *const (), _nth_parent: u8, _name: &[u8]) {
    unreachable!()
}

/// Miri-provided extern function to print (from the interpreter, not the
/// program) the contents of a section of program memory, as bytes. Bytes
/// written using this function will emerge from the interpreter's stdout.
pub fn miri_write_to_stdout(_bytes: &[u8]) {
    unreachable!()
}

/// Miri-provided extern function to print (from the interpreter, not the
/// program) the contents of a section of program memory, as bytes. Bytes
/// written using this function will emerge from the interpreter's stderr.
pub fn miri_write_to_stderr(_bytes: &[u8]) {
    unreachable!()
}

/// Miri-provided extern function to allocate memory from the interpreter.
///
/// This is useful when no fundamental way of allocating memory is
/// available, e.g. when using `no_std` + `alloc`.
pub fn miri_alloc(_size: usize, _align: usize) -> *mut u8 {
    unreachable!()
}

/// Miri-provided extern function to deallocate memory.
pub fn miri_dealloc(_ptr: *mut u8, _size: usize, _align: usize) {
    unreachable!()
}

/// Convert a path from the host Miri runs on to the target Miri interprets.
/// Performs conversion of path separators as needed.
///
/// Usually Miri performs this kind of conversion automatically. However, manual conversion
/// might be necessary when reading an environment variable that was set on the host
/// (such as TMPDIR) and using it as a target path.
///
/// Only works with isolation disabled.
///
/// `in` must point to a null-terminated string, and will be read as the input host path.
/// `out` must point to at least `out_size` many bytes, and the result will be stored there
/// with a null terminator.
/// Returns 0 if the `out` buffer was large enough, and the required size otherwise.
pub fn miri_host_to_target_path(
    _path: *const core::ffi::c_char,
    _out: *mut core::ffi::c_char,
    _out_size: usize,
) -> usize {
    unreachable!()
}

/// Run the provenance GC. The GC will run automatically at some cadence,
/// but in tests we want to for sure run it at certain points to check
/// that it doesn't break anything.
pub fn miri_run_provenance_gc() {
    unreachable!()
}

/// Miri-provided extern function to promise that a given pointer is properly aligned for
/// "symbolic" alignment checks. Will fail if the pointer is not actually aligned or `align` is
/// not a power of two. Has no effect when alignment checks are concrete (which is the default).
pub fn miri_promise_symbolic_alignment(_ptr: *const (), _align: usize) {
    unreachable!()
}
