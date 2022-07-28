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

## $GF(2^{128})$

We represent elements of $GF(2^{128})$ as `int64*innt64`. Note that $(a,b)$ corresponds to $ax^{64} +b$ where $a,b$ are viewed as polynomials of degree less than 64 as in $GF(2^{64})$. The sign convention is as above. One notes that $x^{128}=x^7+x^2+𝑥+1=135$.

Note that $(ax^{64} +b)(cx^{64} +d = acx^{128}+(ad+bc)x^{64} + bd = a\cdot c\cdot128+(a\cdot d+b\cdot c)x^{64} + b\cdot d$. We then keep reducing these using the fact that $x^{128} = 135$.

Inverses are both computed using the binary algorithm (see for example algorithm 2.49 in `Guide to Elliptic Code Cryptography` by Hankerson Menezes and Vanstone).

The additive FFT is based on [this paper](https://arxiv.org/abs/1404.3458v2).

Note that two of the three constructions only work on Intel machines that have the `_mm_clmulepi64_si128 ` instruction.

`dune exec ./mainn.exe` will compute benchmarks and `dune exec test/test_finite_fields.exe` will test the field properties and the fact that `fft` and `ifft` are inverse to each other.
