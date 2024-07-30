open! Core
open Async
open! Cohttp
open Cohttp_async


type t = {
  price : float
}

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


(* let config_distance_address ?(waypoints = "") place_id_origin place_id_destination transit_mode = 
  "https://maps.googleapis.com/maps/api/directions/json?destination=place_id:" ^ place_id_destination ^
  "&mode=" ^ transit_mode ^ 
  "&origin=place_id:" ^ place_id_origin ^ waypoints
;; *)
let create_kayak_header () = 
  Cohttp.Header.of_list [
    ("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36")
  ]
;;


let call_api () =
  Client.get ~headers:(create_kayak_header ()) (Uri.of_string ("https://www.kayak.com/flights/NYC-SFO/2024-09-18?sort=bestflight_a")) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  (* print_endline body; *)
  return body
;;

let parse_kayak_for_ids js_file = 
  let open Soup in
  let script = List.hd_exn (parse js_file
  $$ "script[id=__R9_HYDRATE_DATA__][type=application/json]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)) in

  let script_json = Jsonaf.of_string script in
  let distance = Jsonaf.member_exn "serverData" script_json |> Jsonaf.member_exn "FlightResultsList" |> Jsonaf.member_exn "sortData" |> Jsonaf.member_exn "bestflight_a" |> Jsonaf.member_exn "price" |> Jsonaf.to_string in

  distance

;;


(* let parse_kayak_for_prices id_list kayak_json *)

(* let get_distance geocode_json : string = 
  try
    let distance = Jsonaf.member_exn "routes" geocode_json |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in
    Jsonaf.to_string distance;
  with 
  | exn -> 
    print_endline (Jsonaf.to_string_hum geocode_json);
    raise exn
    
  ;; *)

