open Core
open Async
open! Cohttp
open Cohttp_async

let config_geocode_address address = 
  let address_word_list =  String.split_on_chars address ~on:[' '] in
  let correct_address = String.concat ~sep:"%20" address_word_list in
  "https://maps.googleapis.com/maps/api/geocode/json?address=" ^ correct_address
;;

let config_distance_address place_id_origin place_id_destination = 
  let transit_mode = "walking" in

  "https://maps.googleapis.com/maps/api/directions/json?destination=place_id:" ^ place_id_destination ^
  "&mode=" ^ transit_mode ^ 
  "&origin=place_id:" ^ place_id_origin
;;

let geocode address =

  let%bind key = Reader.file_contents "/home/ubuntu/api" in
  let address = address ^ "&key=" ^ key in

  Client.get (Uri.of_string (address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string  body in
  return body
;;


let get_place_id json_string : string = 

  let place_id_json = Jsonaf.of_string json_string in
  
  let place_id = Jsonaf.member_exn "results" place_id_json 
  |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "place_id" |> Jsonaf.string_exn in
  place_id  
;;

let get_distance json_string : string = 

  let distance_json = Jsonaf.of_string json_string in
  let distance = Jsonaf.member_exn "routes" distance_json |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in
  Jsonaf.to_string distance
    
;;


let place_id_api address = 
  let origin_address = config_geocode_address (address) in
  let%map place_id_origin_geocode = geocode origin_address in
  get_place_id place_id_origin_geocode

;;

let destination_api place_id_destination place_id_origin = 
  let distance_address = config_distance_address place_id_destination place_id_origin in
  let%map distance_geocode = geocode distance_address in
  get_distance distance_geocode
;;


