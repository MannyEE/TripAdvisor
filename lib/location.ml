open! Core

module Coordinates = struct 
type t =
{
  lat : float ;
  long : float ;
} [@@deriving compare, hash, sexp_of]
end


module T = struct 
type t = 
{
  place_id : string ;
  name : string ;
  formatted_address : string ;
  coordinates : Coordinates.t
} [@@deriving compare, hash, sexp_of]

end
include T
include Hashable.Make_plain(T)
include Comparable.Make_plain(T)