open! Core

module T = struct
  type t = 
  { code : string
  ; name : (string[@compare.ignore])
  ; city : (string option[@compare.ignore])
  ; country : (string option[@compare.ignore])
  ; keywords : (string option[@compare.ignore])
  } [@@deriving sexp_of, compare, hash]
end
  include T
  include Hashable.Make_plain(T)
  include Comparable.Make_plain(T)