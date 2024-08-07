open! Core

type t = {
  price : int
  ; duration : Time_ns.Span.t
} [@@deriving sexp]

(* let (<) (x : t) (y : t) method =  *)
let zero = {price = 0; duration = Time_ns.Span.zero}

let (+) x y = {price = x.price + y.price; duration = Time_ns.Span.(+) x.duration y.duration}


module Comparing_duration = struct

  type nonrec t = t
  let zero = zero
  let (+) = (+)

  let (<) (x : t) (y : t) = 
    Time_ns.Span.(<) x.duration y.duration
end

module Comparing_price = struct
  type nonrec t = t
  let zero = zero
  let (+) = (+)

  let (<) (x : t) (y : t) = 
    Int.(<) x.price y.price
end

