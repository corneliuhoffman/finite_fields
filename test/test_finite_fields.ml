open Finite_fields.Finite_field
open QCheck2
let i128gen =  Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64
 
let noint128_gen = Gen.map (fun (a,b) -> Stdint.Uint128.(logxor (shift_left (of_int64 a) 64) (of_int64 b))) i128gen
(* let i128print = Field128ASM.to_string *)
let array_gen64 = Gen.array_size (Gen.return 16)   Gen.int64
let array_gen128 = Gen.array_size (Gen.return 16)   i128gen
let array_gen128noit = Gen.array_size (Gen.return 16)   noint128_gen
let test_assoc (type a)(module F: S with type t = a) (gen:a Gen.t) =
  Test.make
    ~name:"associativity"
    ~print:Print.(triple F.to_string F.to_string F.to_string)
    Gen.(triple gen gen gen)
    (fun (a, b, c) ->
      let left = F.mult (F.mult a b) c in
      let right = F.mult a (F.mult b c) in
      left = right)


let test_distr  (type a)(module F: S with type t = a) (gen:a Gen.t)  =
  Test.make
    ~name:"distrib"
    Gen.(triple gen gen gen)
    (fun (a, b, c) ->
      let left = F.mult a (F.add b c) in
      let right = F.add (F.mult a b) (F.mult a c) in
      left = right)


let test_com (type a)(module F: S with type t = a) (gen:a Gen.t) =
  Test.make
    ~name:"commutativity"
    Gen.(pair gen gen)
    (fun (a, b) -> F.mult a b = F.mult b a)

 

let test_ident (type a)(module F: S with type t = a) (gen:a Gen.t)  =
  Test.make ~name:"identity" gen (fun a ->
      F.mult a @@ F.one = a)

let test_inv (type a)(module F: S with type t = a) (gen:a Gen.t) =
  Test.make ~name:"inverse" ~count:1000 gen (fun a ->
      assume (a <> F.zero) ;

      let inv =
        try F.inv a
        with _ ->
          Format.printf "inv=%s\n" @@ F.to_string @@ a ;
          a
      in
    F.one = F.mult a inv)


let test_fft128 = Test.make  ~name:"FFT128"
       
       array_gen128
        (fun d-> Format.printf "size =%i" @@ Array.length d;let d1 = Int128FFT.fft d in
        let d2 = Int128FFT.ifft d1 in

  d = d2)

  let test_fft64 = Test.make  ~name:"FFT64"
       
       array_gen64
        (fun d-> Format.printf "size =%i" @@ Array.length d;let d1 = Int64FFT.fft d in
        let d2 = Int64FFT.ifft d1 in

  d = d2)
  let test_fft128noint = Test.make  ~name:"no innntel FFT64"
       
       array_gen128noit
        (fun d-> Format.printf "size =%i" @@ Array.length d;let d1 = No_int_FFT.fft d in
        let d2 = No_int_FFT.ifft d1 in

  d = d2)

let test64=
  List.map
    QCheck_alcotest.to_alcotest
    [test_assoc (module Field64ASM) Gen.int64 ;test_distr (module Field64ASM) Gen.int64;test_ident (module Field64ASM) Gen.int64;test_com (module Field64ASM) Gen.int64; test_inv(module Field64ASM) Gen.int64]
let test128=
  List.map
    QCheck_alcotest.to_alcotest
    [test_assoc (module Field128ASM) i128gen ;test_distr (module Field128ASM) i128gen;test_ident (module Field128ASM) i128gen;test_com (module Field128ASM) i128gen; test_inv (module Field128ASM) i128gen]

let test128noint=
  List.map
    QCheck_alcotest.to_alcotest
    [test_assoc (module No_intel_Field128ASM) noint128_gen ;test_distr (module No_intel_Field128ASM) noint128_gen;test_ident (module No_intel_Field128ASM) noint128_gen;test_com (module No_intel_Field128ASM) noint128_gen; test_inv (module No_intel_Field128ASM) noint128_gen]


(* let field_tests =
  List.map
    QCheck_alcotest.to_alcotest
    [test_assoc (module Field128ASM) i128gen "assoc 128";test_assoc (module Field64ASM) Gen.int64 "assoc 64";test_distr (module Field64ASM) Gen.int64;test_distr1; test_ident (module Field64ASM) Gen.int64;test_ident1; test_com (module Field64ASM) Gen.int64;test_com1; test_inv(module Field64ASM) Gen.int64;
test_inv1] *)
let fft_tests =
  List.map
    QCheck_alcotest.to_alcotest
    [test_fft64; test_fft128;test_fft128noint]

let () = Alcotest.run "field" [("field of size 2^64", test64);  ("field of size 2^128", test128); ("field of size 2^128 no intel", test128noint);("fft", fft_tests)]
