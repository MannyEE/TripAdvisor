open! Core
open Async

module Row = struct
  type t = 
  { code : string
  ; name : string
  ; city : string
  } [@@deriving sexp_of]
  let parse () : t Delimited_kernel.Read.t =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open code = at_header "code" ~f:Fn.id
    and name = at_header "name" ~f:Fn.id
    and city = at_header "city" ~f:Fn.id in
    { code ; name ; city}
  ;;
end

let read_csv ~filename =
  let%bind file = Reader.open_file filename in
  Delimited.Read.pipe_of_reader (Row.parse ()) file |> Pipe.to_list
;;

