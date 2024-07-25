open! Core
module T = struct 
type t = 
{
  place_id : string ;
  name : string ;
  formatted_address : string ;
} [@@deriving compare, hash, sexp_of]

end
include T
include Hashable.Make_plain(T)
include Comparable.Make_plain(T)