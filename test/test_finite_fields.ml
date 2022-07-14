open Finite_fields.Finite_field
open QCheck2
let i128gen =  Gen.map2 (fun a b -> (a,b)) Gen.int64 Gen.int64
 
  (* Gen.map (fun a -> Stdint.Uint128.of_int64 a) Gen.int64 *)
let i128print = fun (a,b) -> Int64.to_string a ^ "; " ^ Int64.to_string b
(* 
let test_assoc =
  Test.make
    ~name:"GF128_associativity"
    ~print:Print.(triple i128print i128print i128print)
    Gen.(triple i128gen i128gen i128gen)
    (fun (a, b, c) ->
      let left = mult_rij128 (mult_rij128 a b) c in
      let right = mult_rij128 a (mult_rij128 b c) in
      left = right) *)
let test_assoc =
  Test.make
    ~name:"GF128_associativity"
    ~print:Print.(triple i128print i128print i128print)
    Gen.(triple i128gen i128gen i128gen)
    (fun (a, b, c) ->
      let left = multiplication (multiplication a b) c in
      let right = multiplication a (multiplication b c) in
      left = right)

let test_distr =
  Test.make
    ~name:"GF128distrib"
    Gen.(triple i128gen i128gen i128gen)
    (fun (a, b, c) ->
      let left = multiplication a (addition b c) in
      let right = addition (multiplication a b) (multiplication a c) in
      left = right)

let test_com =
  Test.make
    ~name:"GF128commutativity"
    Gen.(pair i128gen i128gen)
    (fun (a, b) -> multiplication a b = multiplication b a)

let test_ident =
  Test.make ~name:"GF128identity" i128gen (fun a ->
      multiplication a @@ one = a)

let test_inv =
  Test.make ~name:"GF128inverse" ~count:1000 i128gen (fun a ->
      assume (a <> zero) ;

      let inv =
        try inverse a
        with _ ->
          Format.printf "inv=%s\n" @@ to_string @@ a ;
          a
      in
    one = multiplication a inv)

let tests =
  List.map
    QCheck_alcotest.to_alcotest
    [test_assoc;test_distr;test_ident; test_com; test_inv
    ]

let () = Alcotest.run "field" [("generic_field", tests)]
