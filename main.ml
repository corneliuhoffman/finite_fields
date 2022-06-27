open Finite_fields.Finite_field
open Stdint



let () =
  (* let _ =
       Benchmark.throughput1
         10
         (fun (a, b) -> table_mult 64 a b)
         (100080340, 12340000)
     in *)
let t = precompute_coef 20 (Stdint.Uint128.of_int 123) in
let gen () = QCheck2.( Gen.generate1 @@ Gen.map (fun a -> Stdint.Uint128.of_int64 a) Gen.int64) in
let d = Array.init (1 lsl 20) (fun _ -> gen ()) in 
let l =Stdint.Uint128.of_int 123 in
Format.printf "donesetup ";

  let a = Stdint.Uint128.of_int 127537 in
  let c = Stdint.Uint128.((max_int / a) - a) in
  let _ = Benchmark.throughput1 10 (fun (a, b) -> mult_rij128 a b) (a, c) in
  let _ = Benchmark.throughput1 10 (fun a -> inv_rij128 a) a in
  let _ = Benchmark.throughput1 10 (fun _ -> let mem = Hashtbl.create 500000 in List.init (1 lsl 20) (fun x -> let c = Uint128.of_int x in delta 20 0 0 c l d t mem)) () in

  (* let _ = Benchmark.throughput1 10 (fun a -> inv 64 a) 100080340 in *)
  ()
