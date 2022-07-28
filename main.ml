open Finite_fields.Finite_field
open QCheck2
let i128gen =  Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64
 
let noint128_gen = Gen.map (fun (a,b) -> Stdint.Uint128.(logxor (shift_left (of_int64 a) 64) (of_int64 b))) i128gen

let () =

let gen64 () = (Gen.generate1 ~rand:(Random.State.make_self_init ()) Gen.int64) in
let gen128 () = ( Gen.generate1  ~rand:(Random.State.make_self_init ()) @@ Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64) in
let gen128noint () = QCheck2.( Gen.generate1  ~rand:(Random.State.make_self_init ()) noint128_gen) in
let _ = Benchmark.throughput1 ~name:"mult64" 10 (fun (a, b) -> Field64ASM.mult a b) (gen64(), gen64 ()) in ();
let _ = Benchmark.throughput1 ~name:"inv64" 10 (fun a -> Field64ASM.inv a) @@ gen64 () in ();
let _ = Benchmark.throughput1 ~name:"add64" 10 (fun (a, b) -> Field64ASM.add a b) (gen64(), gen64 ()) in (); 
let d = Array.init (1 lsl 15) (fun _ -> gen64 ()) in 
let _ = Benchmark.throughput1 ~name:"F64fft" 10 (fun () ->  let _ = Int64FFT.fft d in ()) () in



let _ = Benchmark.throughput1 ~name:"mult128" 10 (fun (a, b) -> Field128ASM.mult a b) (gen128(), gen128 ()) in ();
let _ = Benchmark.throughput1 ~name:"inv128" 10 (fun a -> Field128ASM.inv a) @@ gen128 () in ();
let _ = Benchmark.throughput1 ~name:"add128" 10 (fun (a, b) -> Field128ASM.add a b) (gen128(), gen128 ()) in (); 
let d = Array.init (1 lsl 15) (fun _ -> gen128 ()) in 
let _ = Benchmark.throughput1 ~name:"F128fft" 10 (fun () ->  let _ = Int128FFT.fft d in ()) () in

let _ = Benchmark.throughput1 ~name:"mult128nni" 10 (fun (a, b) -> No_intel_Field128ASM.mult a b) (gen128noint(), gen128noint ()) in ();
let _ = Benchmark.throughput1 ~name:"inv128ni" 10 (fun a -> No_intel_Field128ASM.inv a) @@ gen128noint () in ();
let _ = Benchmark.throughput1 ~name:"add128ni" 10 (fun (a, b) -> No_intel_Field128ASM.add a b) (gen128noint(), gen128noint ()) in (); 
let d = Array.init (1 lsl 15) (fun _ -> gen128noint ()) in 
let _ = Benchmark.throughput1 ~name:"F128fftni" 10 (fun () ->  let _ = No_int_FFT.fft d in ()) () in


()


