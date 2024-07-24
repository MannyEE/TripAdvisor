open Core
open Async
open! Cohttp
open Cohttp_async

(* let create_waypoints_address waypoints_list =
  List.fold waypoints_list ~init:"&waypoints="
;; *)
(* let api = Lazy_deferred.create (fun () -> Reader.file_contents "/home/ubuntu/api" ) *)
let config_geocode_address ~street_address = 
  (* let%map key = Lazy_deferred.force_exn api in *)
  let address_word_list =  String.split_on_chars street_address ~on:[' '] in
  let correct_address = String.concat ~sep:"%20" address_word_list in
  "https://maps.googleapis.com/maps/api/geocode/json?address=" ^ correct_address
;;

let config_distance_address ?(waypoints = "") place_id_origin place_id_destination transit_mode = 
  "https://maps.googleapis.com/maps/api/directions/json?destination=place_id:" ^ place_id_destination ^
  "&mode=" ^ transit_mode ^ 
  "&origin=place_id:" ^ place_id_origin ^ waypoints
;;

let api = Lazy_deferred.create (fun () -> Reader.file_contents "/home/ubuntu/api" )
let geocode ~configured_address =
  
  let%bind key = Lazy_deferred.force_exn api in

  let configured_address = configured_address ^ "&key=" ^ key in
  (* let%bind configured_address = config_geocode_address ~street_address in *)
  (* print_endline configured_address; *)

  Client.get (Uri.of_string (configured_address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  return body
;;


let get_place_id json_string : string = 

  try 

  let place_id_json = Jsonaf.of_string json_string in
  
  let place_id = Jsonaf.member_exn "results" place_id_json 
  |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "place_id" |> Jsonaf.string_exn in
  place_id  

  with 
  | exn -> 
    print_endline json_string;
    raise exn


;;
let get_formatted_address json_string : string = 

  try 

  let place_id_json = Jsonaf.of_string json_string in
  
  let place_id = Jsonaf.member_exn "results" place_id_json 
  |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "formatted_address" |> Jsonaf.string_exn in
  place_id  

  with 
  | exn -> 
    print_endline json_string;
    raise exn
;;

let get_distance json_string : string = 
  try

  let distance_json = Jsonaf.of_string json_string in
  let distance = Jsonaf.member_exn "routes" distance_json |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in
  Jsonaf.to_string distance;

  with 
  | exn -> 
    print_endline json_string;
    raise exn
    
;;

let get_location (name : string) : Location.t Deferred.t = 
  let config_name = config_geocode_address ~street_address:name in
  let%map geocode = geocode ~configured_address:config_name in 
  let place_id = get_place_id geocode in 
  let formatted_address = get_formatted_address geocode in 
  {
    Location.place_id = place_id ; name = name ; formatted_address = formatted_address
  }
;;

let place_id_api address = 
  let origin_address = config_geocode_address ~street_address:(address) in
  let%map place_id_origin_geocode = geocode ~configured_address:origin_address in
  get_place_id place_id_origin_geocode
;;

let destination_api ~(destination : Location.t) ~(origin : Location.t) transit_mode = 
  let distance_address = config_distance_address destination.place_id origin.place_id transit_mode in
  let%map distance_geocode = geocode ~configured_address:distance_address in
  Time_ns.Span.of_int_sec (Int.of_string (get_distance distance_geocode))
;;


let print_maps_address (directions : Location.t list) = 
  (* let%map (formatted_directions : string list) = Deferred.List.map ~how:`Sequential directions ~f:(fun str-> 
    let%map json_str = geocode str in
    get_formatted_address json_str
  ) in *)
  let formatted_directions = List.map directions ~f:(fun loc -> String.map loc.name ~f:(fun ch -> match ch with ' ' -> '+' | _ -> ch)) in
  print_endline (List.fold formatted_directions ~init:"https://www.google.com/maps/dir/" ~f:(fun accum_str str -> 
  accum_str ^ str ^ "/"
  ));
;;