(* Generated by ocaml-rs *)

open! Bigarray

(* file: lib.rs *)

external mul: int64 -> int64-> int64-> int64  -> (int64 * int64) = "mul"
external mul64: int64 -> int64->  int64  = "mul64"
external mulbyshift: int64 -> int64-> (int64 * int64) = "mulbyshift"
external multiplication: int64 -> int64->int64->int64-> (int64 * int64) = "multiplication"
external inverse: int64 -> int64-> (int64 * int64) = "inverse"
external addition: int64 -> int64->int64->int64-> (int64 * int64) = "addition"
