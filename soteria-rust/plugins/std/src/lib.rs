pub use std::*;

pub mod process {
    pub use std::process::*;

    #[inline(always)]
    pub fn abort() -> ! {
        soteria::panic("Function abort() was invoked")
    }

    #[inline(always)]
    pub fn exit(_code: i32) -> ! {
        soteria::panic("Function exit() was invoked")
    }
}

#[macro_export]
macro_rules! assert {
    ($cond:expr $(,)?) => {
        soteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
    };
    ($cond:expr, $($arg:tt)+) => {{
        soteria::assert(!!$cond, concat!(stringify!($($arg)+)));
    }};
}

#[macro_export]
macro_rules! assert_eq {
    ($left:expr, $right:expr $(,)?) => ({
        soteria::assert(($left) == ($right), concat!("assertion failed: ", stringify!($left == $right)));
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        assert!(($left) == ($right), $($arg)+);
    });
}

#[macro_export]
macro_rules! assert_ne {
    ($left:expr, $right:expr $(,)?) => ({
        soteria::assert(($left) != ($right), concat!("assertion failed: ", stringify!($left != $right)));
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        assert!(($left) != ($right), $($arg)+);
    });
}

#[macro_export]
macro_rules! debug_assert {
    ($($x:tt)*) => ({ $crate::assert!($($x)*); })
}

#[macro_export]
macro_rules! debug_assert_eq {
    ($($x:tt)*) => ({ $crate::assert_eq!($($x)*); })
}

#[macro_export]
macro_rules! debug_assert_ne {
    ($($x:tt)*) => ({ $crate::assert_ne!($($x)*); })
}

#[macro_export]
macro_rules! print {
    ($($x:tt)*) => {{ let _ = format_args!($($x)*); }};
}

#[macro_export]
macro_rules! eprint {
    ($($x:tt)*) => {{ let _ = format_args!($($x)*); }};
}

#[macro_export]
macro_rules! println {
    () => { $crate::print!("\n") };
    ($($x:tt)*) => {{ let _ = format_args!($($x)*); }};
}

#[macro_export]
macro_rules! eprintln {
    () => { $crate::eprint!("\n") };
    ($($x:tt)*) => {{ let _ = format_args!($($x)*); }};
}

#[macro_export]
macro_rules! unreachable {
    ($($msg:literal)? $(,)?) => (
        soteria::panic(concat!("internal error: entered unreachable code: ", $($msg)?))
    );
    ($($msg:expr)? $(,)?) => (
        soteria::panic(concat!("internal error: entered unreachable code: ", stringify!($($msg)?)))
    );
    ($fmt:expr, $($arg:tt)*) => {{
        soteria::panic(concat!("internal error: entered unreachable code: ",
        stringify!($fmt, $($arg)*)))}};
}

#[macro_export]
macro_rules! panic {
    () => (
        soteria::panic("explicit panic")
    );
    ($msg:literal $(,)?) => ({
        soteria::panic(concat!($msg))
    });
    ($msg:expr $(,)?) => ({
        soteria::panic(stringify!($msg));
    });
    ($($arg:tt)+) => {{
        soteria::panic(stringify!($($arg)+));
    }};
}
