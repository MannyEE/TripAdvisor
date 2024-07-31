open! Core
open Async

module Row = struct
  type t = 
  { code : string
  ; name : string
  ; city : string option
  ; keywords : string option
  } [@@deriving sexp_of]
  let parse () : t Delimited_kernel.Read.t =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open code = at_header_opt "code" ~f:Fn.id
    and iata_code = at_header_opt "iata_code" ~f:Fn.id
    and name = at_header "name" ~f:Fn.id
    and city = at_header_opt "city" ~f:Fn.id
    and municipality = at_header_opt "municipality" ~f:Fn.id
    and keywords = at_header_opt "keywords" ~f:Fn.id in
    let city = Option.first_some city municipality in
    let code = Option.first_some code iata_code |> Option.value_exn |> String.strip in

    if String.is_empty code then failwith "No code" else
    { code ; name ; city ; keywords}
  ;;
  let convert_to_string (row : t) = 
    row.code ^ ", " ^ row.name ^ ", " ^ (Option.value(row.city) ~default:"") ^ ", " ^ (Option.value(row.keywords) ~default:"")
  ;;
end

let read_csv ~filename =
  let%bind file = Reader.open_file filename in
  Delimited.Read.pipe_of_reader ~header:`Yes ~on_invalid_row:(Delimited.Read.On_invalid_row.skip) (Row.parse ()) file |> Pipe.to_list
;;

let get_airport_code (location : string) = 
  location
;;