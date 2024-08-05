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
  Client.get ~headers:(create_kayak_header ()) (Uri.of_string (address)) >>= fun (resp, body) ->
    print_s [%sexp (resp :Response.t)];
  let%bind body = Body.to_string body in
  print_endline body;
  return body
;;

let parse_kayak_for_prices js_file ~optimization = 

  print_endline js_file;

  let open Soup in
  let script = List.hd_exn (parse js_file
  $$ "script[id=__R9_HYDRATE_DATA__][type=application/json]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)) in

  let script_json = Jsonaf.of_string script in

    (* print_s [%sexp (script_json : Jsonaf.t)]; *)


  let price_str = Jsonaf.member_exn "serverData" script_json 
  |> Jsonaf.member_exn "FlightResultsList" 
  |> Jsonaf.member_exn "sortData" 
  |> Jsonaf.member_exn "bestflight_a" 
  |> Jsonaf.member_exn optimization 
  |> Jsonaf.to_string in

  (* print_endline price_str; *)

  let price = Int.of_string (String.strip price_str ~drop:(fun char -> 
    Char.equal char '"' || Char.equal char '$')) in

  price


  ;;

let plane_api ~origin_city_code ~destination_city_code ~date ~(optimization : string) = 

  ignore optimization;
  let date_string = (Int.to_string (Date.year date)) ^ "-" ^ (zfill (Int.to_string (Month.to_int (Date.month date))) 2) ^ "-" ^ (Int.to_string (Date.day date)) in
  let kayak_address = config_kayak_address ~origin_city_code ~destination_city_code date_string in
  let%bind kayak_json = call_api kayak_address in

  print_endline kayak_json;
  (* let%bind kayak_json =  Reader.file_contents "kayak" in
  let price = parse_kayak_for_prices kayak_json ~optimization in
  return price *)
  return 2

;;
