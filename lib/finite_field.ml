(* note that x^128 + x^7 + x^2 + x + 1 is the definig polynomial for F*)
(* let shift = Z.(~$135) *)

(** 𝑥^8+𝑥^4+𝑥^3+𝑥+1 coresponds with 27 1 + 2 + 8 + 16 =*)

(** ly(x^128 + x^7 + x^2 + x + 1 corrsponds to 1 +2 +4 + 128 =135, GF(2))*)

open Stdint

let mult_rij a b =
  let a, b = if a < b then (a, b) else (b, a) in
  let rec aux acc a b =
    if a = 0 then acc
    else if a = 1 then b lxor acc
    else
      let new_acc = if a mod 2 = 1 then acc lxor b else acc in
      let new_b = b lsl 1 in
      let new_a = a lsr 1 in
      if new_b < 256 then aux new_acc new_a new_b
      else
        let new_b = new_b lxor 256 lxor 27 in
        aux new_acc new_a new_b
  in
  aux 0 a b

(** TODO: use sliding windows*)
let mult_rij128 a b =
  let a, b = if a < b then (a, b) else (b, a) in
  let rec aux acc a b =
    let shift = Uint128.of_int 135 in
    if a = Uint128.zero then acc
    else if a = Uint128.one then Uint128.logxor b acc
    else
      let new_acc =
        if Uint128.(logand a @@ one) = Uint128.one then Uint128.logxor acc b
        else acc
      in
      let new_b = Uint128.shift_left b 1 in
      let new_a = Uint128.shift_right a 1 in
      if Uint128.shift_right new_b 127 = Uint128.one then
        aux new_acc new_a new_b
      else
        let new_b = Uint128.logxor new_b shift in
        aux new_acc new_a new_b
  in
  aux Uint128.zero a b

let square_rij128 a = mult_rij128 a a

(* let inv_rij128 a =
   let rec aux acc n a =
     if n = Uint128.one then acc
     else
       let new_a = square_rij128 a in
       let new_acc = mult_rij128 acc new_a in
       let new_n = Uint128.shift_right n 1 in
       aux new_acc new_n new_a
   in
   aux Uint128.one Uint128.max_int a *)

let div_by_power n a =
  let x = Uint128.(pred (shift_left one n)) in
  Uint128.logand a x = Uint128.zero

let add_f g =
  let shift = Uint128.of_int 135 in
  let first = Uint128.shift_right (Uint128.logxor shift g) 1 in
  Uint128.logxor (Uint128.shift_left Uint128.one 127) first

let rec reduce_a a g1 g2 =
  if Uint128.logand a Uint128.one = Uint128.one then
    let f = add_f a in
    let prov_g2 = Uint128.logxor g1 g2 in
    let new_g2 =
      if Uint128.logand prov_g2 Uint128.one = Uint128.zero then
        Uint128.shift_right prov_g2 1
      else add_f prov_g2
    in
    (a, f, g1, new_g2)
  else
    let new_a = Uint128.shift_right a 1 in
    let new_g1 =
      if Uint128.logand g1 Uint128.one = Uint128.zero then
        Uint128.shift_right g1 1
      else add_f g1
    in
    reduce_a new_a new_g1 g2

(** TODO: use "almost inverses" or otehr methods*)
let inv_rij128 a =
  let rec aux u v g1 g2 =
    match (u = Uint128.one, v = Uint128.one) with
    | true, _ -> g1
    | _, true -> g2
    | _ -> (
        match
          ( Uint128.logand u Uint128.one = Uint128.zero,
            Uint128.logand v Uint128.one = Uint128.zero )
        with
        | true, _ ->
            let new_u = Uint128.shift_right u 1 in
            let new_g1 =
              if Uint128.logand g1 Uint128.one = Uint128.zero then
                Uint128.shift_right g1 1
              else add_f g1
            in
            aux new_u v new_g1 g2
        | _, true ->
            let new_v = Uint128.shift_right v 1 in
            let new_g2 =
              if Uint128.logand g2 Uint128.one = Uint128.zero then
                Uint128.shift_right g2 1
              else add_f g2
            in
            aux u new_v g1 new_g2
        | _ ->
            if Uint128.compare u v > 0 then
              let new_u = Uint128.logxor u v in
              let new_g1 = Uint128.logxor g1 g2 in
              aux new_u v new_g1 g2
            else
              let new_v = Uint128.logxor u v in
              let new_g2 = Uint128.logxor g1 g2 in
              aux u new_v g1 new_g2)
  in
  let u, v, g1, g2 = reduce_a a Uint128.one Uint128.zero in
  aux u v g1 g2

let rec aq acc n a =
  if n = 1 then acc
  else
    let new_a = 2 * a in
    let new_acc = new_a + acc in
    let new_n = n / 2 in
    aq new_acc new_n new_a

let s_map_rij = List.init 256 (fun x -> mult_rij x x lxor x)

let omega_rij = 64

let split n a =
  let a1 = a lsr (n / 2) in
  let a2 = a lxor (a1 lsl (n / 2)) in
  (a1, a2)

(* let table = Hashtbl.create 500000

   let _ =
     for i = 0 to (256 * 256) - 1 do
       let a, b = split 16 i in
       Hashtbl.add table i (mult_rij a b)
     done *)

let rec mult_by_x n a =
  if n = 8 then mult_rij a omega_rij
  else
    let a1, a2 = split n a in
    ((a1 lxor a2) lsl (n / 2)) lxor mult_by_x (n / 2) a1

let omega n =
  match n with
  | 1 -> 1
  | 2 -> 2
  | 4 -> 14
  | 8 -> 254
  | 16 -> 65534
  | 32 -> 4294967294
  | _ -> 0

let rec mult n a b =
  if n = 1 then a land b
  else
    let o = omega (n / 2) in
    let a1, a2 = split n a in
    let b1, b2 = split n b in
    let a1b1 = mult (n / 2) a1 b1 in
    let a2b2 = mult (n / 2) a2 b2 in
    let rest = mult (n / 2) (a1 lxor a2) (b1 lxor b2) in
    let left = (rest lxor a2b2) lsl (n / 2) in
    let right = a2b2 lxor mult (n / 2) o a1b1 in
    left lxor right

let table = Hashtbl.create 500000

let _ =
  for i = 0 to (256 * 256) - 1 do
    let a, b = split 16 i in
    Hashtbl.add table i (mult 8 a b)
  done

let inv_table = Hashtbl.create 500000

let _ =
  for i = 1 to 255 do
    for j = 1 to 255 do
      if Hashtbl.find table ((i lsl 8) lxor j) = 1 then
        Hashtbl.add inv_table i j
    done
  done

(* left omega n =
   let o = 1 lsl (n / 2) in
   if n <= 2 then o else mult n o (mult n o o) *)
(*
     let mult n a b =
       let rec aux n a b =
         if n = 1 then a land b
         else
           let a1, a2 = split n a in
           let b1, b2 = split n b in
           let a1b1 = aux (n / 2) a1 b1 in
           let a2b2 = aux (n / 2) a2 b2 in
           let rest = aux (n / 2) (a1 lxor a2) (b1 lxor b2) in
           let left = (rest lxor a2b2) lsl (n / 2) in
           let right = a2b2 lxor mult_by_x (n / 2) a1b1 in
           left lxor right
       in
       aux n a b *)
let rec table_mult n a b =
  if n = 8 then Hashtbl.find table ((a lsl 8) lxor b)
  else
    let o = omega (n / 2) in
    let a1, a2 = split n a in
    let b1, b2 = split n b in
    let a1b1 = table_mult (n / 2) a1 b1 in
    let a2b2 = table_mult (n / 2) a2 b2 in
    let rest = table_mult (n / 2) (a1 lxor a2) (b1 lxor b2) in
    let left = (rest lxor a2b2) lsl (n / 2) in
    let right = a2b2 lxor mult (n / 2) o a1b1 in
    left lxor right

let square n a = table_mult n a a

let rec inv n x =
  if n = 8 then Hashtbl.find inv_table x
  else
    let a, b = split n x in
    let a2 = square (n / 2) a in
    let b2 = square (n / 2) b in
    let ab = mult (n / 2) a b in
    let w = mult_by_x (n / 2) a2 lxor (b2 lxor ab) in
    let s = inv (n / 2) w in
    let left = mult (n / 2) a s in
    let right = mult (n / 2) (a lxor b) s in
    (left lsl (n / 2)) lxor right

let mult128 (a1, a2) (b1, b2) =
  let a1b1 = table_mult 64 a1 b1 in
  let a2b2 = table_mult 64 a2 b2 in
  let rest = table_mult 64 (a1 lxor a2) (b1 lxor b2) in
  let left = (rest lxor a2b2) lsl 32 in
  let right = a2b2 lxor mult_by_x 32 a1b1 in
  (left, right)

let w i x =
  let rec aux acc k x =
    if k = Uint128.shift_left Uint128.one i then acc
    else
      let new_acc = mult_rij128 acc (Uint128.logxor x k) in
      aux new_acc (Uint128.succ k) x
  in
  aux Uint128.one Uint128.zero x

let precompute_coef n l =
  let h = 1 lsl n in
  let table = Hashtbl.create (h * n) in
  for i = 0 to n - 1 do
    let wi = inv_rij128 @@ w i (Uint128.shift_left Uint128.one i) in
    let wl = w i l in
    for c = 0 to (h lsr i) - 1 do
      Hashtbl.add
        table
        (i, Uint128.of_int (c lsl i))
        (mult_rij128 wi (Uint128.logxor wl @@ w i (Uint128.of_int (c lsl i))))
    done
  done ;
  table

let rec delta logh i m c l d coefs mem =
  (* let h = Uint128.shift_left Uint128.one logh in *)
  assert (m < 1 lsl (i + 1)) ;
  assert (div_by_power i c) ;
  (* Format.printf "i = %i; m=%i; c = %s\n" i m (Uint128.to_string c) ; *)
  (* assert (Uint128.compare c h < 0) ; *)
  try Hashtbl.find mem (i, m, c)
  with _ ->
    if i = logh then (
      Hashtbl.add mem (i, m, c) d.(m) ;
      d.(m))
    else if div_by_power (i + 1) c then (
      let left = delta logh (i + 1) m c l d coefs mem in
      let right = delta logh (i + 1) (m lsl i) c l d coefs mem in
      let coeficient = Hashtbl.find coefs (i, c) in
      let result = Uint128.logxor left @@ mult_rij128 coeficient right in
      Hashtbl.add mem (i, m, c) result ;
      result)
    else
      let left =
        delta
          logh
          i
          m
          (Uint128.logxor c (Uint128.shift_left Uint128.one i))
          l
          d
          coefs
          mem
      in
      let right =
        delta
          logh
          (i + 1)
          (m lsl i)
          (Uint128.logxor c (Uint128.shift_left Uint128.one i))
          l
          d
          coefs
          mem
      in
      let result = Uint128.logxor left right in
      Hashtbl.add mem (i, m, c) result ;
      result

module type S = sig
  type t = Uint128.t

  val l_for_fft : t

  val one : t

  val zero : t

  val shift_left : t -> int -> t

  val shift_right : t -> int -> t

  val of_int : int -> t

  val to_string : t -> string

  val mult : t -> t -> t

  val square : t -> t

  val add : t -> t -> t

  val inv : t -> t
end

module Make (Field : S) : sig
  include S with type t = Uint128.t

  val fft : t array -> t array

  val ffti : t array -> t array
end = struct
  include Field

  let div_by_power n a = shift_left (shift_right a n) n = a

  (** produce the element $\omega_i$*)

  (** this computes the subspace polinomials $W_i(x)= \prod_{i=0}{2^i}(x-i) *)

  (** this precomputes a hash table that cntains at position (i c) the value $
      $W_i(c +l)/W_i(2^i)$*)
  let precomp_coef = precompute_coef 16 l_for_fft

  let rec delta logh i m c l d coefs mem =
    (* let h = Uint128.shift_left Uint128.one logh in *)
    (* assert (m < 1 lsl (i + 1)) ;
       assert (div_by_power i c) ; *)
    
    (* assert (Uint128.compare c h < 0) ; *)
    try Hashtbl.find mem (i, m, c)
    with _ ->
      if i = logh then (
        Hashtbl.add mem (i, m, c) d.(m) ;
        d.(m))
      else if div_by_power (i + 1) c then (
        let left = delta logh (i + 1) m c l d coefs mem in
        let right = delta logh (i + 1) (m + (1 lsl i)) c l d coefs mem in
        let coeficient = Hashtbl.find coefs (i, c) in
        let result = add left @@ mult coeficient right in

        Hashtbl.add mem (i, m, c) result ;
        result)
      else
        let left = delta logh i m (add c (shift_left one i)) l d coefs mem in
        let right =
          delta logh (i + 1) (m + (1 lsl i)) (add c (shift_left one i)) l d coefs mem
        in
        let result = add left right in
        Hashtbl.add mem (i, m, c) result ;
        result

  let rec rev_delta logh i m c l d coefs mem =
  if  i=1 && m =1 then Format.printf " here i = %i; m =%i c= %i\n" i m @@ Uint128.to_int c;
    match (i,m) with
    | (0,0) ->d.(Uint128.to_int c)
    
    |_ ->
          try (let x = Hashtbl.find mem (i, m, c) in
x
          )
    with _ ->
      if c= zero then rev_delta logh 0 m c l d coefs mem 
      else
      if (div_by_power (i-1)  c) then
           (   
        let left = rev_delta logh (i-1) (m - (1 lsl (i-1))) (add c (shift_left one (i-1))) l d coefs mem in
        let right =
          rev_delta logh (i - 1) (m - (1 lsl (i-1))) (add c (shift_left one (i-1))) l d coefs mem
        in
        let result = add left right in
        Hashtbl.add mem (i, m, c) result ;
        result)
       
      else
        (        

let left = rev_delta logh (i - 1) m c l d coefs mem in 
       let right = rev_delta logh i (m + (1 lsl (i-1))) c l d coefs mem in
       let coeficient = Hashtbl.find coefs (i-1, c) in
       let result = add left @@ mult coeficient right in
        Hashtbl.add mem (i, m, c) result ;
        result)
        

       



  let fft d =
    let logh = Z.log2up (Z.of_int (Array.length d)) in
    let mem = Hashtbl.create 500000 in
    Array.init (1 lsl logh) (fun x ->
      
        let c = of_int x in
        delta logh 0 0 c l_for_fft d precomp_coef mem)

  let ffti d = let logh = Z.log2up (Z.of_int (Array.length d)) in
    let mem = Hashtbl.create 500000 in
    Array.init (1 lsl logh) (fun x ->
      Format.printf "\n\nnox x = %i" x;
        
        rev_delta logh logh x (of_int x) l_for_fft d precomp_coef mem)
end

module Fi128 = Make (struct
  type t = Uint128.t

  let l_for_fft = Uint128.of_int 124

  let mult = mult_rij128

  let inv = inv_rij128

  let zero = Uint128.zero

  let one = Uint128.one

  let shift_left = Uint128.shift_left

  let shift_right = Uint128.shift_right

  let add = Uint128.logxor

  let square x = mult x x

  let of_int = Uint128.of_int

  let to_string = Uint128.to_string
end)

(* module Rij : S = struct
     type t = int

     let field_size = 8

     let of_string s =
       let a = Z.of_string s in
       assert (a >= Z.of_int 0 && Z.(leq a (pow (of_int 2) field_size))) ;
       Z.to_int a

     let to_string = Int.to_string

     let mult_by_x = mult_rij omega_rij

     let mult a b =  Hashtbl.find table ((a lsl 8) lxor b)

     let square a = mult a a

     let add a b = a lxor b

     let inv a = assert (a <>0 ); Hashtbl.find table a
     let omega = omega_rij
   end *)
