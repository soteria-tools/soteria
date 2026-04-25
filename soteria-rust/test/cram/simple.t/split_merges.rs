// Taken from
// https://github.com/rust-lang/miri/blob/master/tests/pass/union-overwrite.rs

#[repr(C)]
#[derive(Clone, Copy)]
struct Pair<T, U>(T, U);
#[repr(C)]
#[derive(Clone, Copy)]
struct Triple<T>(T, T, T);

#[repr(C)]
union U<A: Copy, B: Copy> {
    a: Pair<A, A>,
    b: B,
}

#[repr(C)]
union W<A: Copy, B: Copy> {
    a: A,
    b: B,
}

#[cfg(target_endian = "little")]
#[soteria::test]
fn endianness() {
    unsafe {
        let mut u = U::<u8, u16> { b: 0xDE_DE };
        u.a.0 = 0xBE;
        assert_eq!(u.b, 0xDE_BE);

        let mut u = U::<u16, u32> { b: 0xDEAD_DEAD };
        u.a.0 = 0xBEEF;
        assert_eq!(u.b, 0xDEAD_BEEF);

        let mut u = U::<u32, u64> {
            b: 0xDEADBEEF_DEADBEEF,
        };
        u.a.0 = 0xBAADF00D;
        assert_eq!(u.b, 0xDEADBEEF_BAADF00D);

        let mut w = W::<Pair<Triple<u8>, u8>, u32> { b: 0xDEAD_DEAD };
        w.a.0 = Triple(0, 0, 0);
        assert_eq!(w.b, 0xDE00_0000);

        let mut w = W::<Pair<u8, Triple<u8>>, u32> { b: 0xDEAD_DEAD };
        w.a.1 = Triple(0, 0, 0);
        assert_eq!(w.b, 0x0000_00AD);
    }
}

#[soteria::test]
fn uninit_gap() {
    union MyU32 {
        as_bytes: [u8; 4],
        as_u32: u32,
        as_uninit: (),
    }
    unsafe {
        let mut x = MyU32 { as_uninit: () };
        x.as_bytes[0] = 0x12;
        x.as_bytes[1] = 0x34;
        // x.as_bytes[2] is uninitialized
        x.as_bytes[3] = 0x78;
        assert_eq!(x.as_u32, 0x1234_5678);
    };
}
