(* note that x^128 + x^7 + x^2 + x + 1 is the definig polynomial for F*)
(* let shift = Z.(~$135) *)

(** 𝑥^8+𝑥^4+𝑥^3+𝑥+1 coresponds with 27 1 + 2 + 8 + 16 =*)

(** ly(x^128 + x^7 + x^2 + x + 1 corrsponds to 1 +2 +4 + 128 =135, GF(2))*)

open Stdint

module type S = sig
  type t

  val l_for_fft : t

  val one : t

  val zero : t

  val of_int : int -> t

  val to_int : t -> int

  val truncate : t -> int64

  val to_string : t -> string

  val mult : t -> t -> t

  val square : t -> t

  val add : t -> t -> t

  val inv : t -> t
end

module Make (Field : S) : sig
  type t = Field.t

  val table_of_coef : (int * t, t) Hashtbl.t

  val fft : t array -> t array

  val ifft : t array -> t array
end = struct
  include Field

  let subspace_poly i x =
    let rec aux acc k x =
      if k = 1 lsl i then acc
      else
        let new_acc = Field.mult acc (add x (of_int k)) in
        aux new_acc (succ k) x
    in
    aux one 0 x

  let precompute_coeff n l =
    let h = 1 lsl n in
    let table = Hashtbl.create (h * n) in
    for i = 0 to n - 1 do
      let wi = inv @@ subspace_poly i @@ of_int @@ (1 lsl i) in
      let wl = subspace_poly i l in
      for c = 0 to (h lsr i) - 1 do
        Hashtbl.add
          table
          (i, of_int (c lsl i))
          (mult wi (add wl @@ subspace_poly i @@ of_int (c lsl i)))
      done
    done ;
    table

  let table_of_coef = precompute_coeff 18 @@ of_int 124

  let div_by_power n a =
    let x = Int64.(pred (shift_left one n)) in
    Int64.logand (truncate a) x = 0L

  let rec delta logh i m c l d coefs mem =
    if i = logh then (
      Hashtbl.add mem (i, m, c) d.(m) ;
      d.(m))
    else if div_by_power (i + 1) c then (
      let left =
        try Hashtbl.find mem (i + 1, m, c)
        with _ -> delta logh (i + 1) m c l d coefs mem
      in
      let right =
        try Hashtbl.find mem (i + 1, m + (1 lsl i), c)
        with _ -> delta logh (i + 1) (m + (1 lsl i)) c l d coefs mem
      in
      let coeficient = Hashtbl.find coefs (i, c) in
      let result = add left @@ mult coeficient right in
      Hashtbl.add mem (i, m, c) result ;

      result)
    else
      let s = add c @@ of_int @@ (1 lsl i) in
      let left =
        try Hashtbl.find mem (i, m, s)
        with _ -> delta logh i m s l d coefs mem
      in
      let right =
        try Hashtbl.find mem (i + 1, m + (1 lsl i), s)
        with _ -> delta logh (i + 1) (m + (1 lsl i)) s l d coefs mem
      in
      let result = add left right in
      Hashtbl.add mem (i, m, c) result ;
      result

  let fft d =
    let logh = Z.log2up (Z.of_int (Array.length d)) in
    let mem = Hashtbl.create 500000 in

    Array.init (1 lsl logh) (fun x ->
        let c = of_int x in

        delta logh 0 0 c l_for_fft d table_of_coef mem)

  let rec rev_delta logh i m c l d coefs mem =
    match (i, m) with
    | 0, 0 -> d.(to_int c)
    | _ ->
        if Hashtbl.mem mem (i, m, c) then Hashtbl.find mem (i, m, c)
        else if m >= 1 lsl (i - 1) then (
          let left =
            rev_delta logh (i - 1) (m - (1 lsl (i - 1))) c l d coefs mem
          in
          let right =
            rev_delta
              logh
              (i - 1)
              (m - (1 lsl (i - 1)))
              (add c @@ of_int (1 lsl (i - 1)))
              l
              d
              coefs
              mem
          in
          let result = add left right in
          Hashtbl.add mem (i, m, c) result ;
          result)
        else
          let left = rev_delta logh (i - 1) m c l d coefs mem in

          let right = rev_delta logh i (m + (1 lsl (i - 1))) c l d coefs mem in
          let coeficient =
            if add c l < of_int (1 lsl (i - 1)) then zero
            else Hashtbl.find coefs (i - 1, c)
          in
          let result = add left @@ mult coeficient right in
          Hashtbl.add mem (i, m, c) result ;
          result

  let ifft d =
    let logh = Z.log2up (Z.of_int (Array.length d)) in
    let mem = Hashtbl.create 500000 in

    Array.init (1 lsl logh) (fun x ->
        rev_delta logh logh x zero l_for_fft d table_of_coef mem)
end

module Field128ASM : S with type t = int64 * int64 = struct
  type t = int64 * int64

  let l_for_fft = (0L, 124L)

  let one = (0L, 1L)

  let zero = (0L, 0L)

  let of_int a = (0L, Int64.of_int a)

  let to_int (a, b) = if a <> 0L then failwith "too big" else Int64.to_int b

  let to_string (a, b) = Format.sprintf "%Li, %Li" a b

  let truncate = snd

  let mult (a, b) (c, d) =
    let x, y = Lib.multiplication a b c d in
    (x, y)

  let add (a, b) (c, d) =
    (* let x, y = Lib.addition a b c d in *)
    (Int64.logxor a c, Int64.logxor b d)

  let square a = mult a a

  let inv (a, b) =
    let x, y = Lib.inverse a b in
    (x, y)
end

module Field64ASM : S with type t = int64 = struct
  type t = int64

  let l_for_fft = 124L

  let one = 1L

  let zero = 0L

  let of_int = Int64.of_int

  let to_int = Int64.to_int

  let to_string = Int64.to_string

  let truncate a = a

  let mult = Lib.mul64

  let add = Int64.logxor

  let square a = mult a a

  let odd a = Int64.logand a 1L = 1L

  let add_f g =
    if odd g then
      let shift = of_int 27 in
      let first = Int64.shift_right_logical (Int64.logxor shift g) 1 in
      Int64.logxor (Int64.shift_left one 63) first
    else Int64.shift_right_logical g 1

  let rec reduce a g =
    if odd a then (a, g)
    else
      let new_a = add_f a in

      let new_g = add_f g in
      reduce new_a new_g

  let larger_power a b = a < 0L || (a > b && b >= 0L)

  let inv a =
    let rec aux u v g1 g2 =
      match (u = one, v = one) with
      | true, _ -> g1
      | _, true -> g2
      | _ -> (
          match (odd u, odd v) with
          | false, _ -> let new_u, new_g1 = reduce u g1 in

                        aux new_u v new_g1 g2
          | _, false -> let new_v, new_g2 = reduce v g2 in

                        aux u new_v g1 new_g2
          | _ ->
              if larger_power u v then
                let new_u, new_g1 =
                  reduce (Int64.logxor u v) (Int64.logxor g1 g2)
                in
                aux new_u v new_g1 g2
              else
                let new_v, new_g2 =
                  reduce (Int64.logxor u v) (Int64.logxor g1 g2)
                in

                aux u new_v g1 new_g2)
    in

    let u, g1 = reduce a one in
    if u = one then g1
    else
      let v = add_f u in
      let g2 = if odd g1 then add_f g1 else Int64.shift_right_logical g1 1 in
      aux u v g1 g2
end

module No_intel_Field128ASM : S with type t = Uint128.t = struct
  type t = Uint128.t

  let l_for_fft = Uint128.of_int 124

  let one = Uint128.of_int 1

  let zero = Uint128.of_int 0

  let of_int = Uint128.of_int

  let to_int = Uint128.to_int

  let to_string = Uint128.to_string

  let truncate = Uint128.to_int64

  let mult a b =
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
        if Uint128.shift_right b 127 = Uint128.zero then aux new_acc new_a new_b
        else
          let new_b = Uint128.logxor new_b shift in
          aux new_acc new_a new_b
    in
    aux Uint128.zero a b

  let add = Uint128.logxor

  let square a = mult a a

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
  let inv a =
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
end

module No_int_FFT = Make (No_intel_Field128ASM)
module Int128FFT = Make (Field128ASM)
module Int64FFT = Make (Field64ASM)
