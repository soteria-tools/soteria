#[soteria::test]
fn basic() {
    let target = "world";
    let number = 11;
    let foo = format!("Hello, {}! I like the number {}", target, number);
    assert_eq!(foo, "Hello, world! I like the number 11");
}

#[soteria::test]
fn formats_u128() {
    let j: u128 = 1 << 67;
    assert_eq!("147573952589676412928", format!("{}", j));
    assert_eq!("80000000000000000", format!("{:x}", j));
    assert_eq!("20000000000000000000000", format!("{:o}", j));
}

#[soteria::test]
fn positional_args() {
    let a = "a";
    let b = "b";
    assert_eq!("a b a", format!("{0} {1} {0}", a, b));
    let x = 1;
    let y = 2;
    assert_eq!("2 1", format!("{1} {0}", x, y));
}

#[soteria::test]
fn named_args() {
    let a = 3;
    let b = 4;
    assert_eq!("3 4", format!("{x} {y}", x = a, y = b));
    let greet = "hi";
    let who = "there";
    assert_eq!(
        "hi there",
        format!("{greet} {who}", who = who, greet = greet)
    );
}

#[soteria::test]
fn captured_identifiers() {
    let name = "Alice";
    let age = 30;
    assert_eq!("Alice is 30", format!("{name} is {age}"));
}

#[soteria::test]
fn escaped_braces() {
    let five = 5;
    let hundred = 100;
    assert_eq!("{}", format!("{{}}"));
    assert_eq!("{5}", format!("{{{}}}", five));
    assert_eq!("100%", format!("{}%", hundred));
}

#[soteria::test]
fn width() {
    let n = 5;
    let s = "5";
    let hi = "hi";
    assert_eq!("    5", format!("{:5}", n));
    assert_eq!("5    ", format!("{:<5}", n));
    assert_eq!("  5  ", format!("{:^5}", n));
    assert_eq!("5    ", format!("{:5}", s));
    assert_eq!("hi   ", format!("{:5}", hi));
}

#[soteria::test]
fn width_from_arg() {
    let n = 5;
    let w = 5;
    assert_eq!("    5", format!("{:1$}", n, w));
    assert_eq!("    5", format!("{:width$}", n, width = w));
    assert_eq!("    5", format!("{0:1$}", n, w));
}

#[soteria::test]
fn fill_and_align() {
    let n = 5;
    let m = 42;
    assert_eq!("**5**", format!("{:*^5}", n));
    assert_eq!("xx5", format!("{:x>3}", n));
    assert_eq!("5--", format!("{:-<3}", n));
    assert_eq!("0042", format!("{:0>4}", m));
}

#[soteria::test]
fn sign() {
    let p = 5;
    let n = -5;
    assert_eq!("+5", format!("{:+}", p));
    assert_eq!("-5", format!("{:+}", n));
    let i: i32 = 5;
    assert_eq!("+5", format!("{:+}", i));
}

#[soteria::test]
fn zero_pad() {
    let p = 5;
    let n = -5;
    assert_eq!("00005", format!("{:05}", p));
    assert_eq!("-0005", format!("{:05}", n));
    assert_eq!("+0005", format!("{:+05}", p));
}

#[soteria::test]
fn precision_float() {
    let pi = 3.14159;
    let h = 1.5;
    assert_eq!("3.14", format!("{:.2}", pi));
    // FIXME: for now we comment these out bc they're painfully slow.
    // assert_eq!("3.142", format!("{:.3}", pi));
    // assert_eq!("3", format!("{:.0}", pi));
    // assert_eq!("1.50", format!("{:.2}", h));
}

#[soteria::test]
fn precision_from_arg() {
    let pi = 3.14159;
    let prec = 2;
    assert_eq!("3.14", format!("{:.1$}", pi, prec));
    assert_eq!("3.14", format!("{:.prec$}", pi, prec = prec));
    assert_eq!("3.14", format!("{:.*}", prec, pi));
}

#[soteria::test]
fn precision_string() {
    let s = "hello";
    assert_eq!("hel", format!("{:.3}", s));
    assert_eq!("hello", format!("{:.10}", s));
}

#[soteria::test]
fn width_and_precision() {
    let pi = 3.14159;
    assert_eq!("  3.14", format!("{:6.2}", pi));
    assert_eq!("3.14  ", format!("{:<6.2}", pi));
    assert_eq!("003.14", format!("{:06.2}", pi));
}

#[soteria::test]
fn hex() {
    let n = 255;
    assert_eq!("ff", format!("{:x}", n));
    assert_eq!("FF", format!("{:X}", n));
    assert_eq!("0xff", format!("{:#x}", n));
    assert_eq!("0xFF", format!("{:#X}", n));
    assert_eq!("0x0ff", format!("{:#05x}", n));
}

#[soteria::test]
fn octal() {
    let n = 511;
    assert_eq!("777", format!("{:o}", n));
    assert_eq!("0o777", format!("{:#o}", n));
}

#[soteria::test]
fn binary() {
    let n = 5;
    assert_eq!("101", format!("{:b}", n));
    assert_eq!("0b101", format!("{:#b}", n));
    assert_eq!("0b00000101", format!("{:#010b}", n));
}

#[soteria::test]
fn exponent() {
    let a = 150.0;
    let b = 1.0;
    assert_eq!("1.5e2", format!("{:e}", a));
    // FIXME: for now we comment these out bc they're painfully slow.
    // assert_eq!("1.5E2", format!("{:E}", a));
    // assert_eq!("1e0", format!("{:e}", b));
}

#[soteria::test]
fn debug() {
    let s = "hello";
    let c = 'a';
    let n = 5;
    let arr = [1, 2, 3];
    let tup = (1, "a");
    assert_eq!("\"hello\"", format!("{:?}", s));
    assert_eq!("'a'", format!("{:?}", c));
    assert_eq!("5", format!("{:?}", n));
    assert_eq!("[1, 2, 3]", format!("{:?}", arr));
    assert_eq!("(1, \"a\")", format!("{:?}", tup));
}

#[soteria::test]
fn debug_pretty() {
    let arr = [1, 2, 3];
    assert_eq!("[\n    1,\n    2,\n    3,\n]", format!("{:#?}", arr));
}

#[soteria::test]
fn debug_option() {
    let some = Some(5);
    let none = Option::<i32>::None;
    let ok = Result::<i32, i32>::Ok(1);
    let err = Result::<i32, i32>::Err(2);
    assert_eq!("Some(5)", format!("{:?}", some));
    assert_eq!("None", format!("{:?}", none));
    assert_eq!("Ok(1)", format!("{:?}", ok));
    assert_eq!("Err(2)", format!("{:?}", err));
}

#[soteria::test]
fn char_and_bool() {
    let c = 'a';
    let t = true;
    let f = false;
    assert_eq!("a", format!("{}", c));
    assert_eq!("true", format!("{}", t));
    assert_eq!("false", format!("{}", f));
    assert_eq!("  a", format!("{:>3}", c));
}

#[soteria::test]
fn floats_display() {
    // let one = 1.0;
    // let h = 1.5;
    // let nh = -0.5;
    let inf = f64::INFINITY;
    let ninf = f64::NEG_INFINITY;
    let nan = f64::NAN;
    // assert_eq!("1", format!("{}", one));
    // assert_eq!("1.5", format!("{}", h));
    // assert_eq!("-0.5", format!("{}", nh));
    assert_eq!("inf", format!("{}", inf));
    assert_eq!("-inf", format!("{}", ninf));
    assert_eq!("NaN", format!("{}", nan));
}

#[soteria::test]
fn negative_hex_padding() {
    let a = -5i8;
    let b = -5i32;
    assert_eq!("fb", format!("{:x}", a as u8));
    assert_eq!("fffffffb", format!("{:x}", b as u32));
}

#[soteria::test]
fn mixed_complex() {
    let name = "x";
    let pi = 3.14159;
    let n = 255;
    assert_eq!(
        "x = +003.14 (0xff)",
        format!("{name} = {:+07.2} ({:#x})", pi, n)
    );
}

#[soteria::test]
fn multiple_uses_same_arg() {
    let one = 1;
    assert_eq!("1 1 1", format!("{0} {0} {0}", one));
    let v = 7;
    assert_eq!("7 and 7", format!("{v} and {v}"));
}

#[soteria::test]
fn nested_args_width_precision() {
    let pi = 3.14159;
    let w = 6;
    let p = 2;
    assert_eq!("  3.14", format!("{:1$.2$}", pi, w, p));
    assert_eq!("  3.14", format!("{val:w$.p$}", val = pi, w = w, p = p));
}

#[soteria::test]
fn empty_and_literal() {
    assert_eq!("", format!(""));
    assert_eq!("just text", format!("just text"));
    assert_eq!("tab\tnewline\n", format!("tab\tnewline\n"));
}

#[soteria::test]
fn address() {
    let foo = 11;
    let _x = format!("{:p}", &foo);
}
