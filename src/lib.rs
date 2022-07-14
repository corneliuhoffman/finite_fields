// #[cfg(all(
//     feature = "clmul",
//     target_arch = "x86_64",
//     target_feature = "sse2",
//     target_feature = "pclmulqdq"
// ))]

use core::arch::x86_64::_mm_clmulepi64_si128;
// use core::arch::x86_64::__m128i;
use core::arch::x86_64::{
    __m128i, _mm_and_si128, _mm_bslli_si128, _mm_bsrli_si128, _mm_cvtsi64x_si128,
    _mm_extract_epi64, _mm_set_epi64x, _mm_xor_si128,
};
use std::arch::x86_64::_mm_srli_epi64;
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
        //  println!("bsh -{:?}; ash= {:?}; z={:?}; r= {:?}\n",bsh,ash, z, r);

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

pub fn addf(x: __m128i) -> __m128i {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let sh = _mm_cvtsi64x_si128(135);
        let first = _mm_xor_si128(sh, x);
        let shift_first = _mm_bsrli_si128(first, 1);
        let top = _mm_bslli_si128(_mm_cvtsi64x_si128(1), 127);
        _mm_xor_si128(top, shift_first)
    }
}

pub fn odd(x: __m128i) -> bool {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let one = _mm_cvtsi64x_si128(1);
        println!("{:?}, {:?}\n", x, _mm_and_si128(x, one));
        _mm_extract_epi64(_mm_and_si128(x, one), 0) != 0
    }
}

pub fn odd(x: __m128i, y:__m128i) -> bool{
     #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
   unsafe {let (a,b) = (_mm_extract_epi64(x,0),_mm_extract_epi64(x,1));
   let (c,d) = (_mm_extract_epi64(y,0),_mm_extract_epi64(y,1));
(a,b)==(c,d)}


}

pub fn reduce(a: __m128i, g1: __m128i, g2: __m128i) -> (__m128i, __m128i, __m128i, __m128i) {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        if odd(a) {
            let f = addf(a);
            let prov_g2 = _mm_xor_si128(g1, g2);
            let newg2 = if odd(prov_g2) {
                addf(prov_g2)
            } else {
                _mm_bsrli_si128(prov_g2, 1)
            };
            (a, f, g1, newg2)
        } else {
            let new_a = _mm_bsrli_si128(a, 1);
            let new_g1 = if odd(g1) {
                addf(g1)
            } else {
                _mm_bsrli_si128(g1, 1)
            };
            reduce(new_a, new_g1, g2)
        }
    }
}

pub fn inv(x) -> __m128i
{ #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {let one = _mm_cvtsi64x_si128(1);
    let zero = _mm_cvtsi64x_si128(0);

    fn aux (u: __m128i,v:__m128i, g1: __m128i, g2: __m128i)->__m128i
    {
        match (odd(u), off(v)) {

            (true, _) => {let new_u = _mm_srli_epi64(u,1);
            let new_g1 = if odd(g1) {_mm_srli_epi64(g1,1)} else {addf(g1)};
            
        }
            
        }
    }}
} 