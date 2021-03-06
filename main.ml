open Finite_fields.Finite_field
open Stdint



let () =
  (* let _ =
       Benchmark.throughput1
         10
         (fun (a, b) -> table_mult 64 a b)
         (100080340, 12340000)
     (* in *)
let gen () = QCheck2.( Gen.generate1  ~rand:(Random.State.make_self_init ()) @@ Gen.map (fun a -> Stdint.Uint128.of_int64 a) Gen.int64) in
let d = Array.init (1 lsl 16) (fun _ -> gen ()) in 
(* 
  let a = Stdint.Uint128.of_int 127537 in
  let c = Stdint.Uint128.((max_int / a) - a) in
  let _ = Benchmark.throughput1 10 (fun (a, b) -> mult_rij128 a b) (a, c) in
  let _ = Benchmark.throughput1 10 (fun a -> inv_rij128 a) a in *)
  (* let _ = Benchmark.throughput1 10 (fun _ ->  let _ = Fi128.ffti d in ()) () in *)
let d1 = Fi128.ffti d in
Format.printf "%b" (d = (Fi128.fft d1)); *)
let z= (Int64.shift_left 1L 63) in
 let _ = Benchmark.throughput1 ~name:"asm multiplication" 10 (fun () -> multiplication (z, 2L) (z, 67L) ) () in
 let a = Stdint.Uint128.(logxor (shift_left one 127) (of_int64 2L)) in
  let c =  Stdint.Uint128.(logxor (of_int64 67L )(shift_left one 127))in
let _ = Benchmark.throughput1 ~name:"ocaml multiplication" 10 (fun (a, b) -> mult_rij128 a b) (a, c) in ();
let _ = Benchmark.throughput1 ~name:"asm inverse" 1 (fun () -> inverse (z, 2L)) () in ();
let _ = Benchmark.throughput1  1 (fun () -> inv_rij128 a) () in ();

let gen () = QCheck2.( Gen.generate1  ~rand:(Random.State.make_self_init ()) @@ Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64) in
let d = Array.init (1 lsl 16) (fun _ -> gen ()) in 
let _ = Benchmark.throughput1 ~name:"fft" 1 (fun () ->  let _ = fft d in ()) () in
 


