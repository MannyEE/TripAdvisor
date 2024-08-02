open! Core
open Async

  let parse_airports (country_map : string String.Map.t) : Airport.t Delimited_kernel.Read.t =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open code = at_header "iata_code" ~f:Fn.id
    and name = at_header "name" ~f:Fn.id
    and city = at_header_opt "municipality" ~f:Fn.id
    and country = at_header "iso_country" ~f:(Map.find country_map)
    and keywords = at_header_opt "keywords" ~f:Fn.id in
    if String.is_empty code then failwith "No code" else
    Airport.{ code ; name ; city ; country ; keywords}
  ;;

  let parse_citycodes (country_map : string String.Map.t) : Airport.t Delimited_kernel.Read.t =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open code = at_header "code" ~f:Fn.id
    and name = at_header "name" ~f:Fn.id
    and city = at_header_opt "city" ~f:Fn.id 
    and country = at_header "country" ~f:(Map.find country_map) in
    if String.is_empty code then failwith "No code" else
    Airport.{ code ; name ; city ; country ; keywords = None}
  ;;

  let convert_to_string (row : Airport.t) = 
    let comma = ", " in
    row.code ^ comma ^ row.name ^ comma ^ (Option.value(row.city) ~default:"") ^ comma ^ (Option.value(row.country) ~default:"") ^ comma ^ (Option.value(row.keywords) ~default:"")
  ;;

let read_csv ~filename ~country_map ~parse =
  let%bind file = Reader.open_file filename in
  
  Delimited.Read.pipe_of_reader ~header:`Yes ~on_invalid_row:(Delimited.Read.On_invalid_row.skip) (parse country_map) file |> Pipe.to_list
;;

let map_countries ~filename = 
  let%bind file = Reader.open_file filename in
  let parse () =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open code = at_header "code" ~f:Fn.id
    and name = at_header "name" ~f:Fn.id in
    if String.is_empty code then failwith "No code" else
    (code, name)
  in

  let%bind list = Delimited.Read.pipe_of_reader ~header:`Yes ~on_invalid_row:(Delimited.Read.On_invalid_row.skip) (parse ()) file |> Pipe.to_list in
  return (String.Map.of_alist_exn list)
;;

let get_all_airports () =
  let%bind country_map = map_countries ~filename:"countries.csv" in
  let%bind airports_list = read_csv ~filename:"goated_airports.csv" ~country_map ~parse:parse_airports in 
  let%bind citycodes_list = read_csv ~filename:"citycodes.csv" ~country_map ~parse:parse_citycodes in
  return (airports_list @ citycodes_list)
;;