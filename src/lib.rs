// #[cfg(all(
//     feature = "clmul",
//     target_arch = "x&86_64",
//     target_feature = "sse2",
//     target_feature = "pclmulqdq"
// ))]

use core::arch::x86_64::_mm_clmulepi64_si128;
// use core::arch::x86_64::__m128i;
use core::arch::x86_64::{
    __m128i, _mm_and_si128, _mm_cvtsi64x_si128, _mm_extract_epi64, _mm_set_epi64x, _mm_xor_si128,
};

// use std::time::{ Instant};
//  use std::ops::BitXor;
//   use core::arch::x86_64:: _mm_cvtsi128_si64,;
#[ocaml::func]
#[ocaml::sig("int64 -> int64-> int64-> int64  -> (int64 * int64)")]
pub fn mul(a: i64, b: i64, c: i64, d: i64) -> (i64, i64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let (bd, _ad, _bc, _ac) = (
            _mm_clmulepi64_si128(_mm_set_epi64x(a, b), _mm_set_epi64x(c, d), 0),
            _mm_clmulepi64_si128(_mm_set_epi64x(a, b), _mm_set_epi64x(c, d), 1),
            _mm_clmulepi64_si128(_mm_set_epi64x(a, b), _mm_set_epi64x(c, d), 16),
            _mm_clmulepi64_si128(_mm_set_epi64x(a, b), _mm_set_epi64x(c, d), 17),
        );

        (_mm_extract_epi64(bd, 0), _mm_extract_epi64(bd, 1))
    }
}

pub fn mulbyx(x: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let a = _mm_clmulepi64_si128(x, _mm_cvtsi64x_si128(135), 1);
        let b = _mm_set_epi64x(_mm_extract_epi64(x, 0), 0);
        let z = _mm_xor_si128(a, b);
        z
    }
}

pub fn mulbyshift_x(number: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let sh = _mm_cvtsi64x_si128(135);
        // let number  = _mm_set_epi64x(a,b);
        let (bsh, ash) = (
            _mm_clmulepi64_si128(number, sh, 0),
            _mm_clmulepi64_si128(number, sh, 1),
        );
        let r = mulbyx(ash);
        let z = _mm_xor_si128(bsh, r);

        z
    }
}

#[ocaml::func]
#[ocaml::sig("int64 -> int64-> (int64 * int64)")]
pub fn mulbyshift(a: i64, b: i64) -> (i64, i64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let bd = mulbyshift_x(_mm_set_epi64x(a, b));

        (_mm_extract_epi64(bd, 1), _mm_extract_epi64(bd, 0))
    }
}

pub fn multadd(x: __m128i, y: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let (a, b) = (_mm_extract_epi64(x, 0), _mm_extract_epi64(x, 1));
        let x1 = _mm_xor_si128(_mm_cvtsi64x_si128(a), _mm_cvtsi64x_si128(b));
        let (c, d) = (_mm_extract_epi64(y, 0), _mm_extract_epi64(y, 1));
        let x2 = _mm_xor_si128(_mm_cvtsi64x_si128(c), _mm_cvtsi64x_si128(d));
        _mm_clmulepi64_si128(x1, x2, 0)
    }
}

#[ocaml::func]
#[ocaml::sig("int64 -> int64->int64->int64-> (int64 * int64)")]
pub fn multiplication(a: i64, b: i64, c: i64, d: i64) -> (i64, i64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let x1 = _mm_set_epi64x(a, b);
        let x2 = _mm_set_epi64x(c, d);
        let (bd, ad, bc, ac) = (
            _mm_clmulepi64_si128(x1, x2, 0),
            _mm_clmulepi64_si128(x1, x2, 1),
            _mm_clmulepi64_si128(x1, x2, 16),
            _mm_clmulepi64_si128(x1, x2, 17),
        );
        let adbc = _mm_xor_si128(ad, bc);
        let adbcx = mulbyx(adbc);
        let acsh = mulbyshift_x(ac);
        let acshbd = _mm_xor_si128(acsh, bd);
        let result = _mm_xor_si128(acshbd, adbcx);
        (_mm_extract_epi64(result, 1), _mm_extract_epi64(result, 0))
    }
}
pub fn sh(b: i64) -> i64 {
    if b >= 0 {
        b >> 1
    } else {
        (b ^ i64::MIN) >> 1 ^ (1 << 62)
    }
}

pub fn shift_right(x: __m128i) -> __m128i {
    unsafe {
        let a = _mm_extract_epi64(x, 0);
        let b = _mm_extract_epi64(x, 1);

        let new_b = sh(b);

        let shift = if b & 1 == 1 { i64::MIN } else { 0 };
        let new_a = sh(a) | (shift);
        _mm_set_epi64x(new_b, new_a)
    }
}

pub fn addf(x: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let sh = _mm_cvtsi64x_si128(135);
        let first = _mm_xor_si128(sh, x);
        let shift_first = shift_right(first);
        let top = _mm_set_epi64x(i64::MIN, 0);
        _mm_xor_si128(top, shift_first)
    }
}

pub fn odd(x: __m128i) -> bool {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let one = _mm_cvtsi64x_si128(1);
        _mm_extract_epi64(_mm_and_si128(x, one), 0) != 0
    }
}

pub fn equalm128(x: __m128i, y: __m128i) -> bool {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let (a, b) = (_mm_extract_epi64(x, 0), _mm_extract_epi64(x, 1));
        let (c, d) = (_mm_extract_epi64(y, 0), _mm_extract_epi64(y, 1));
        (a, b) == (c, d)
    }
}

pub fn reduce(a: __m128i, g: __m128i) -> (__m128i, __m128i) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    if odd(a) {
        (a, g)
    } else {
        let new_a = shift_right(a);

        let new_g = if odd(g) { addf(g) } else { shift_right(g) };
        reduce(new_a, new_g)
    }
}

pub fn larger_power(x: __m128i, y: __m128i) -> bool {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let (a, b) = (_mm_extract_epi64(x, 1), _mm_extract_epi64(x, 0));
        let (c, d) = (_mm_extract_epi64(y, 1), _mm_extract_epi64(y, 0));
        a.leading_zeros() < c.leading_zeros()
            || (a == 0 && c == 0 && b.leading_zeros() < d.leading_zeros())
    }
}

pub fn inv(a: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let one = _mm_cvtsi64x_si128(1);
        fn aux(u: __m128i, v: __m128i, g1: __m128i, g2: __m128i) -> __m128i {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            unsafe {
                let one = _mm_cvtsi64x_si128(1);
                match (equalm128(u, one), equalm128(v, one)) {
                    (true, _) => g1,
                    (_, true) => g2,
                    _ => match (odd(u), odd(v)) {
                        (false, _) => {
                            let (new_u, new_g1) = reduce(u, g1);
                            // let new_u = shift_right(u, 1);
                            // let new_g1 = if odd(g1) {
                            //     _mm_srli_epi64(g1, 1)
                            // } else {
                            //     addf(g1)
                            // };
                            aux(new_u, v, new_g1, g2)
                        }
                        (_, false) => {
                            let (new_v, new_g2) = reduce(v, g2);

                            // let new_v = shift_right(v, 1);
                            // let new_g2 = if odd(g2) {
                            //     _mm_srli_epi64(g2, 1)
                            // } else {
                            //     addf(g2)
                            // };
                            aux(u, new_v, g1, new_g2)
                        }
                        _ => {
                            if larger_power(u, v) {
                                let (new_u, new_g1) =
                                    reduce(_mm_xor_si128(u, v), _mm_xor_si128(g1, g2));
                                aux(new_u, v, new_g1, g2)
                            } else {
                                let (new_v, new_g2) =
                                    reduce(_mm_xor_si128(u, v), _mm_xor_si128(g1, g2));

                                aux(u, new_v, g1, new_g2)
                            }
                        }
                    },
                }
            }
        }
        let (u, g1) = reduce(a, one);
        if equalm128(u, one) {
            g1
        } else {
            let v = addf(u);
            let g2 = if odd(g1) { addf(g1) } else { shift_right(g1) };
            aux(u, v, g1, g2)
        }
    }
}

#[ocaml::func]
#[ocaml::sig("int64 -> int64-> (int64 * int64)")]
pub fn inverse(a: i64, b: i64) -> (i64, i64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let x = _mm_set_epi64x(a, b);
        let result = inv(x);
        (_mm_extract_epi64(result, 1), _mm_extract_epi64(result, 0))
    }
}
#[ocaml::func]
#[ocaml::sig("int64 -> int64->int64->int64-> (int64 * int64)")]
pub fn addition(a: i64, b: i64, c: i64, d: i64) -> (i64, i64) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let x = _mm_set_epi64x(a, b);
        let y = _mm_set_epi64x(c, d);
        let result = _mm_xor_si128(x, y);
        (_mm_extract_epi64(result, 1), _mm_extract_epi64(result, 0))
    }
}
