open Finite_fields.Finite_field

let () =
  (* let _ =
       Benchmark.throughput1
         10
         (fun (a, b) -> table_mult 64 a b)
         (100080340, 12340000)
     in *)
  let a = Stdint.Uint128.of_int 127537 in
  let c = Stdint.Uint128.((max_int / a) - a) in
  let _ = Benchmark.throughput1 10 (fun (a, b) -> mult_rij128 a b) (a, c) in
  let _ = Benchmark.throughput1 10 (fun a -> inv_rij128 a) a in
  let _ = Benchmark.throughput1 10 (fun a -> second_inv128 a) a in
  (* let _ = Benchmark.throughput1 10 (fun a -> inv 64 a) 100080340 in *)
  ()
