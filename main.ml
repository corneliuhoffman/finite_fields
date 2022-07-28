open Finite_fields.Finite_field
(* let x = -4744970800384872238L

(* open Stdint *)
  let odd a = Int64.logand a 1L = 1L
 let add_f g =
    if odd g then
      let shift = Int64.of_int 27 in
      let first = Int64.shift_right_logical (Int64.logxor shift g) 1 in
      let r =Int64.logxor (Int64.shift_left 1L 63) first in
                (* Format.printf "g= %a \nfirst= %a,\n r =%a\n " Stdint.Int64.printer_bin g Stdint.Int64.printer_bin first Stdint.Int64.printer_bin (Field64ASM.mult x first);  *)
      r
    else Int64.shift_right_logical g 1

  let rec reduce a g =
    (* Format.printf "\nu= %Li, g = %Li prod = %b\n " a  g (Field64ASM.mult x g = a); *)


    if odd a then (a, g)
    else
      let new_a = add_f a in
 

      let new_g = add_f g in

               
              (* Format.printf "\nistrue= %b\n "  (new_a = Field64ASM.mult x  new_g); *)


      reduce new_a new_g *)

let () =
let gen64 () = QCheck2.(Gen.generate1 ~rand:(Random.State.make_self_init ()) Gen.int64) in
let gen () = QCheck2.( Gen.generate1  ~rand:(Random.State.make_self_init ()) @@ Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64) in
let _ = Benchmark.throughput1 ~name:"mult64" 10 (fun (a, b) -> Field64ASM.mult a b) (gen64(), gen64 ()) in ();
let _ = Benchmark.throughput1 ~name:"inv64" 10 (fun a -> Field64ASM.inv a) @@ gen64 () in ();
let _ = Benchmark.throughput1 ~name:"add64" 10 (fun (a, b) -> Field64ASM.add a b) (gen64(), gen64 ()) in (); 

let d = Array.init (1 lsl 15) (fun _ -> gen64 ()) in 



let _ = Benchmark.throughput1 ~name:"F64fft" 10 (fun () ->  let _ = Int64FFT.fft d in ()) () in



let _ = Benchmark.throughput1 ~name:"mult128" 10 (fun (a, b) -> Field128ASM.mult a b) (gen(), gen ()) in ();
let _ = Benchmark.throughput1 ~name:"inv128" 10 (fun a -> Field128ASM.inv a) @@ gen () in ();

let _ = Benchmark.throughput1 ~name:"add128" 10 (fun (a, b) -> Field128ASM.add a b) (gen(), gen ()) in (); 
let d = Array.init (1 lsl 15) (fun _ -> gen ()) in 



let _ = Benchmark.throughput1 ~name:"F128fft" 10 (fun () ->  let _ = Int128FFT.fft d in ()) () in

()


