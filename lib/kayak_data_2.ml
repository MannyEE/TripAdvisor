(* open! Core


type t = {
  price : int
  ; duration : Time_ns.Span.t
  ; flight_date : Date.t
  ; request_date : Date.t
} [@@deriving sexp, compare]



(* let (<) (x : t) (y : t) method =  *)
let zero date = {price = 0; duration = Time_ns.Span.zero; flight_date = date; request_date = Date.today ~zone:(Timezone.utc)}

let large = {duration = Time_ns.Span.of_int_day (365); price = 946853; flight_date = Date.of_string "00000000"; request_date = Date.of_string "00000000"}
let (+) x y = {price = x.price + y.price; duration = Time_ns.Span.(+) x.duration y.duration}

let is_large t = 
  [%compare.equal : t] t large
  
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
 *)
