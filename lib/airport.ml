open! Core

module T = struct
  type t = 
  { code : string
  ; name : (string[@compare.ignore])
  ; city : (string option[@compare.ignore])
  ; country : (string option[@compare.ignore])
  ; keywords : (string option[@compare.ignore])
  } [@@deriving sexp, compare, hash]
end
  include T
  include Hashable.Make(T)
  include Comparable.Make_plain(T)

  let convert_to_string (row : t) = 
    let comma = ", " in
    row.code ^ comma ^ row.name ^ comma ^ (Option.value(row.city) ~default:"") ^ comma ^ (Option.value(row.country) ~default:"") ^ comma ^ (Option.value(row.keywords) ~default:"")
  ;;