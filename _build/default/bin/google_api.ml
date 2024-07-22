open Core
open Async
open! Cohttp
open Cohttp_async


let config_geocode_address address = 
  let address_word_list =  String.split_on_chars address ~on:[' '] in
  let correct_address = String.concat ~sep:"%20" address_word_list in

  "https://maps.googleapis.com/maps/api/geocode/json?address=" ^ correct_address ^ "&key="
;;

let config_distance_address place_id_origin place_id_destination = 
  let transit_mode = "walking" in

  "https://maps.googleapis.com/maps/api/directions/json?destination=place_id:" ^ place_id_destination ^
  "&mode=" ^ transit_mode ^ 
  "&origin=place_id:" ^ place_id_origin ^
  "&key="
;;




let geocode address =
  Client.get (Uri.of_string (address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string  body in
  return body
;;



let get_place_id json_string : string = 

  let place_id_json = Jsonaf.of_string json_string in
  
  let place_id = Jsonaf.member_exn "results" place_id_json 
  |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "place_id" |> Jsonaf.string_exn in

  (* print_endline place_id; *)
  place_id  
;;

let get_distance json_string : string = 

  (* print_endline json_string; *)
  (* json_string *)
  let distance_json = Jsonaf.of_string json_string in

  let distance = Jsonaf.member_exn "routes" distance_json |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in

  (* print_endline (Jsonaf.to_string distance); *)

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


(* let config_destination_link address = 
  
  "https://maps.googleapis.com/maps/api/directions/json?origin=Disneyland&destination=Universal+Studios+Hollywood&key="
;; *)

(* let destination =
  Client.get (Uri.of_string (config_destination_link)) >>= fun (_resp, body) ->
  (* let code = resp |> Response.status |> Code.code_of_status in *)
  (* printf "Response code: %d\n" code;
  printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string); *)
  let%bind body =  Body.to_string  body in
  (* printf "Body of length: %d\n" (String.length body); *)
  return body
;;
 *)



