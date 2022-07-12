// #[cfg(all(
//     feature = "clmul",
//     target_arch = "x86_64",
//     target_feature = "sse2",
//     target_feature = "pclmulqdq"
// ))]
 

use core::arch::x86_64::_mm_clmulepi64_si128;
    // use core::arch::x86_64::__m128i;
    use core::arch::x86_64::{_mm_set_epi64x, _mm_extract_epi64, _mm_xor_si128, _mm_cvtsi64x_si128, __m128i};
    // use std::time::{ Instant};
    //  use std::ops::BitXor;
    //   use core::arch::x86_64:: _mm_cvtsi128_si64,;
#[ocaml::func]
#[ocaml::sig("int64 -> int64-> int64-> int64  -> (int64 * int64)")]
pub fn mul (a:i64,b:i64,c:i64,d:i64) ->  (i64,i64) {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{
    let (bd,_ad,_bc,_ac) = 

 (_mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 0),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 1),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 16),
 _mm_clmulepi64_si128(_mm_set_epi64x(a,b),_mm_set_epi64x(c,d) , 17))
;

(_mm_extract_epi64(bd,0),_mm_extract_epi64(bd,1) )
}
   
}


pub fn mulbyx (x:__m128i) ->  __m128i {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{
    let a = _mm_clmulepi64_si128(x,_mm_cvtsi64x_si128(135) , 1);
    let b = _mm_set_epi64x(_mm_extract_epi64(x,0), 0);
    let z = _mm_xor_si128(a,b); 
z
}}
   

pub fn mulbyshift_x (number:__m128i) ->  __m128i {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{
    let sh = _mm_cvtsi64x_si128(135);
    // let number  = _mm_set_epi64x(a,b);
    let (bsh,ash) = 

 (_mm_clmulepi64_si128(number,sh , 0),
 _mm_clmulepi64_si128(number,sh, 1));
    let lowerterm = _mm_cvtsi64x_si128(_mm_extract_epi64(ash,0));
    let higherterm = _mm_clmulepi64_si128(_mm_cvtsi64x_si128(_mm_extract_epi64(ash,1)), sh, 0) ;
    let z = _mm_xor_si128(bsh, lowerterm);
    let r =mulbyx(higherterm);
    let w = _mm_xor_si128(z, r); 
w}}

#[ocaml::func]
#[ocaml::sig("int64 -> int64-> (int64 * int64)")]
pub fn mulbyshift (a:i64,b:i64) ->  (i64,i64) {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{ let bd = mulbyshift_x(_mm_set_epi64x(a,b));

(_mm_extract_epi64(bd,0),_mm_extract_epi64(bd,1) )
}}

pub fn multadd (x:__m128i, y:__m128i) ->  __m128i
   {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{ let(a,b)=(_mm_extract_epi64(x,0),_mm_extract_epi64(x,1) );
    let x1 = _mm_xor_si128(_mm_cvtsi64x_si128(a),_mm_cvtsi64x_si128(b));
    let(c,d)=(_mm_extract_epi64(y,0),_mm_extract_epi64(y,1) );
    let x2 = _mm_xor_si128(_mm_cvtsi64x_si128(c),_mm_cvtsi64x_si128(d));
   _mm_clmulepi64_si128(x1,x2, 0) 
}}

#[ocaml::func]
#[ocaml::sig("int64 -> int64->int64->int64-> (int64 * int64)")]
pub fn multiplication (a:i64,b:i64, c:i64,d:i64) ->  (i64,i64) {
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe{ let x1 = _mm_set_epi64x(a,b);
    let x2 = _mm_set_epi64x(c,d);
    let (bd, ad,bc,ac) = 

 (_mm_clmulepi64_si128(x1,x2 , 0),
 _mm_clmulepi64_si128(x1,x2 , 1),
 _mm_clmulepi64_si128(x1,x2 , 16),
 _mm_clmulepi64_si128(x1,x2 , 17))
;
let adbc= _mm_xor_si128(ad, bc);
let adbcx= mulbyx(adbc);
let acsh= mulbyshift_x(ac);
let acshbd=_mm_xor_si128(acsh,bd); 
let result = _mm_xor_si128(acshbd, adbcx);
(_mm_extract_epi64(result,0),_mm_extract_epi64(result,1) )
}}
