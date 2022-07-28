# Finite fields

This implements additive fast fourier transform for finite binary fields field of size $2^24$ and £2^128$.

To do so it uses the following:

$GF(2^{64})= GF(2)[x]/(x^{64}+x^4+x^3+𝑥+1)$ and $GF(2^{128})= GF(2)[x]/(x^{128}+x^7+x^2+𝑥+1)$. We will use these polynomials in the representation.

## $GF(2^{64})$

We represent the elements of $GF(2^{64})$ as `int64`. Note that these elements are signed and the first bit is the sign. Therefore the number `Int64.min_int` represents the element $x^{63}$ while the element $-1$ represents $x^{63}+x^{62}\cdots x+1$. One notes that $x^{64}=x^4+x^3+𝑥+1=27$.

Addition is just `logxor`.
Multiplication is done via the `core::arch::x86_64::_mm_clmulepi64_si128` in Rust. More precisely we use the following:

$\mbox{clmul}(a,  b)=: a_1x^{64} +b_1$

$a_{2} := a_{1} >>> 60$

$a_{3} := a_1+ (a_{2}<<60)$

$a2sh := \mbox{clmul}(a_2,  (x^{4}+x^{3}+x+1))$

$a_{4} := (a2sh) >>> 4$

$a_{5} := a2sh+ (a\_{4}<<4)$

$result :=\mbox{clmul}( ( a_{4}+a_{3}), (x^{4}+x^{3}+x+1)) + a\_{5}<<60 +b_1$
