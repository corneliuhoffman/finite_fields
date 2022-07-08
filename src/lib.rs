// #[cfg(all(
//     feature = "clmul",
//     target_arch = "x86_64",
//     target_feature = "sse2",
//     target_feature = "pclmulqdq"
// ))]
 

use core::arch::x86_64::_mm_clmulepi64_si128;
    // use core::arch::x86_64::__m128i;
    use core::arch::x86_64::{_mm_set_epi64x, _mm_cvtsi128_si64};
    // use std::time::{ Instant};
    //  use std::ops::BitXor;
    //   use core::arch::x86_64::_mm_clmulepi64_si128;
#[ocaml::func]
#[ocaml::sig("int64 -> int64-> int64-> int64  -> int64")]
pub fn mul (a:i64,b:i64,c:i64,d:i64) ->  i64 {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{
    let (bd,_ad,_bc,_ac) = 

 (_mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 0),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 1),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 16),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 17))
;

_mm_cvtsi128_si64(bd)
}
   
}

