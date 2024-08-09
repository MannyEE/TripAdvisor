open! Core
open Async
open! Cohttp
open Cohttp_async

type t = {
  price : float
}

let zfill s width =
  let to_fill = width - (String.length s) in
  if to_fill <= 0 then s
  else (String.make to_fill '0') ^ s

let formatted_expedia_address city_1 city_2 day month year = 
  "https://www.expedia.com/Flights-Search?flight-type=on&mode=search&trip=oneway&leg1=" ^
  "from%3A" ^ city_1 ^ "%2C+%2Cto%3A" ^ city_2 ^ 
  "%2C+%2Cdeparture%3A" ^ month ^ "%2F" ^ day ^ "%2F" ^ year
;;

let get_flights contents : string list=
  let open Soup in
  parse contents
  $$ "li[data-test-id=offer-listing]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)
;;

let create_kayak_header () = 
  Cohttp.Header.of_list [("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36")]
;;

let config_kayak_address ~origin_city_code ~destination_city_code date = 
  "https://www.kayak.com/flights/" ^ origin_city_code ^ "-" ^ destination_city_code ^ "/" ^ date ^ "?sort=bestflight_a"
;;

let call_api address =
  Client.get ~headers:(create_kayak_header ()) (Uri.of_string (address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  (* print_endline body; *)
  return body
;;

let parse_kayak js_file = 

  let open Soup in
  let script = List.hd_exn (parse js_file
  $$ "script[id=__R9_HYDRATE_DATA__][type=application/json]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)) in

  let script_json = Jsonaf.of_string script in

    (* print_s [%sexp (script_json : Jsonaf.t)]; *)


  let value_str = Jsonaf.member_exn "serverData" script_json 
  |> Jsonaf.member_exn "FlightResultsList" 
  |> Jsonaf.member_exn "results"
  |> Jsonaf.member_exn "sortData" in

  let best_duration = value_str |> Jsonaf.member_exn "duration_a" |> Jsonaf.member_exn "duration" |> Jsonaf.to_string in
  let best_price = value_str |> Jsonaf.member_exn "price_a" |> Jsonaf.member_exn "price" |> Jsonaf.to_string in

  (* print_endline price_str; *)

  let duration_value = 
    let num_list = String.split ~on:' ' best_duration
    |> List.map ~f:(fun num -> 
      String.filter num ~f:Char.is_digit) in
    
    Int.of_string (List.hd_exn num_list) * 60 + Int.of_string (List.last_exn num_list) in

  let price_value = Int.of_string (String.strip best_price ~drop:(fun char -> 
    Char.equal char '"' || Char.equal char '$')) in


  Kayak_data.{duration = Time_ns.Span.of_int_min duration_value; price = price_value}
  ;;

let plane_api ~origin_city_code ~destination_city_code ~date = 

  let date_string = (Int.to_string (Date.year date)) ^ "-" ^ (zfill (Int.to_string (Month.to_int (Date.month date))) 2) ^ "-" ^ (Int.to_string (Date.day date)) in
  let kayak_address = config_kayak_address ~origin_city_code ~destination_city_code date_string in
  let%bind kayak_json = call_api kayak_address in

  (* let%bind kayak_json =  Reader.file_contents "kayak" in *)
  try
    let price = parse_kayak kayak_json in
    let final_msg = origin_city_code ^ " " ^ destination_city_code in
    let () = print_endline (final_msg ^ " " ^(Time_ns.Span.to_string price.duration) ^ " " ^ Int.to_string price.price) in
    return price

  with _ ->
    let final_msg = origin_city_code ^ " " ^ destination_city_code in
    print_endline final_msg;
    (* print_s [%message (err : exn)]; *)
    return Kayak_data.large
  (* return 2 *)

;;

let get_kayak_link ~(best_route : Airport.t list) ~(departure_date : Date.t) ~stay_length = 
  
  (* FIX THIS ONCE WE ADD END OF ROUTE TO TSP/best_route *)

  let route_len = List.length best_route in
  fst (List.foldi best_route ~init:("https://www.kayak.com/flights/", departure_date) ~f:(fun idx (link, departure_date) cur_airport -> 
    if idx = (route_len - 1) then link ^ "?sort=bestflight_a", departure_date else
    let next_idx = (idx + 1) % route_len in
    let next_airport = List.nth_exn best_route next_idx in
    let date_string = (Int.to_string (Date.year departure_date)) ^ "-" ^ (zfill (Int.to_string (Month.to_int (Date.month departure_date))) 2) ^ "-" ^ (zfill (Int.to_string (Date.day departure_date)) 2) in
    (link ^ cur_airport.code ^ "-" ^ next_airport.code ^ "/" ^ date_string ^ "/", Date.add_days departure_date stay_length)
    ))
  ;;

