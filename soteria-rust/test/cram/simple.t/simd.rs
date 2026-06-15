// Exercises the SIMD intrinsics implemented for hashbrown's NEON control group
// and friends: splat, eq, or/and/xor, add/sub/mul, shl/shr, neg, the signed
// comparisons (ge/gt/le/lt) and extract. Run with an aarch64 target (see run.t)
// so the NEON intrinsics are available.
use std::arch::aarch64::*;

fn main() {
    unsafe {
        let a: uint8x8_t = std::mem::transmute([1u8, 2, 3, 4, 5, 6, 7, 8]);
        let b: uint8x8_t = std::mem::transmute([1u8, 9, 3, 9, 5, 9, 7, 9]);

        // comparisons -> per-lane mask (0xff / 0x00)
        let eq = vceq_u8(a, b); // simd_eq
        assert_eq!(vget_lane_u8::<0>(eq), 0xff); // 1 == 1
        assert_eq!(vget_lane_u8::<1>(eq), 0x00); // 2 != 9

        // bitwise: or / and / xor
        assert_eq!(vget_lane_u8::<1>(vorr_u8(eq, eq)), 0x00); // simd_or
        assert_eq!(vget_lane_u8::<0>(vand_u8(a, b)), 1 & 1); // simd_and
        assert_eq!(vget_lane_u8::<1>(veor_u8(a, b)), 2 ^ 9); // simd_xor

        // arithmetic: add / sub / mul
        assert_eq!(vget_lane_u8::<2>(vadd_u8(a, b)), 6); // simd_add
        assert_eq!(vget_lane_u8::<1>(vsub_u8(b, a)), 7); // simd_sub
        assert_eq!(vget_lane_u8::<3>(vmul_u8(a, b)), 36); // simd_mul

        // shifts: shl / shr (logical and arithmetic)
        assert_eq!(vget_lane_u8::<0>(vshl_n_u8::<2>(a)), 4); // simd_shl: 1 << 2
        assert_eq!(vget_lane_u8::<1>(vshr_n_u8::<1>(b)), 4); // simd_shr: 9 >> 1
        let neg: int8x8_t = std::mem::transmute([-8i8; 8]);
        assert_eq!(vget_lane_s8::<0>(vshr_n_s8::<1>(neg)), -4); // arithmetic shr

        // unary negate
        let s: int8x8_t = std::mem::transmute([5i8, -5, 0, 1, -1, 2, -2, 3]);
        assert_eq!(vget_lane_s8::<0>(vneg_s8(s)), -5); // simd_neg

        // signed comparisons: ge / gt / le / lt
        let z: int8x8_t = std::mem::transmute([0i8; 8]);
        assert_eq!(vget_lane_u8::<0>(vcge_s8(s, z)), 0xff); // 5 >= 0
        assert_eq!(vget_lane_u8::<0>(vcgt_s8(s, z)), 0xff); // 5 > 0
        assert_eq!(vget_lane_u8::<1>(vcle_s8(s, z)), 0xff); // -5 <= 0
        assert_eq!(vget_lane_u8::<1>(vclt_s8(s, z)), 0xff); // -5 < 0

        // splat
        assert_eq!(vget_lane_u8::<5>(vdup_n_u8(42)), 42); // simd_splat + simd_extract
    }
}
