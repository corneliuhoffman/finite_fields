open Finite_fields.Finite_field
(* 


let () =
  (* let _ =
       Benchmark.throughput1
         10
         (fun (a, b) -> table_mult 64 a b)
         (100080340, 12340000)
     in *)
let gen () = QCheck2.( Gen.generate1 @@ Gen.map (fun a -> Stdint.Uint128.of_int64 a) Gen.int64) in
let d = Array.init (1 lsl 18) (fun _ -> gen ()) in 
Format.printf "donesetup ";
(* 
  let a = Stdint.Uint128.of_int 127537 in
  let c = Stdint.Uint128.((max_int / a) - a) in
  let _ = Benchmark.throughput1 10 (fun (a, b) -> mult_rij128 a b) (a, c) in
  let _ = Benchmark.throughput1 10 (fun a -> inv_rij128 a) a in *)
  let _ = Benchmark.throughput1 10 (fun _ ->  let _ = Fi128.ffti d in ()) () in
let _d1 = Fi128.ffti d in
(* Format.printf "%b" (d = (Fi128.fft d1)); *)
  (* let _ = Benchmark.throughput1 10 (fun a -> inv 64 a) 100080340 in *)
  () *)
let ()=
 (* let _ =  *)
 
Format.printf "mul = %Li" @@ mulasm 2L 3L 4L 1L 
