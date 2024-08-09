(* open! Core
open Async
open! Cohttp
open Cohttp_async


let config_recommendation_address ~location = 
  
;;


let create_recommendation_header key = 
  Cohttp.Header.of_list [("X-Goog-Api-Key", key) ; ("Content-Type", "application/json") ; ("X-Goog-FieldMask" "places.displayName")]
;;

let api = Lazy_deferred.create (fun () -> Reader.file_contents "/home/ubuntu/api" );;
let call_api ~configured_address =
  let%bind key = Lazy_deferred.force_exn api in

  let uri_with_key = Uri.add_query_param' configured_address ("key" , key) in
  Client.post (uri_with_key) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  return (Jsonaf.of_string body)
;;

let call_api address =
  Client.post ~headers:(create_kayak_header ()) (Uri.of_string (address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  (* print_endline body; *)
  return body
;;


let find_nearby = 
  let origin_address = config_recommendation_address ~street_address:(address) in
  let%map place_id_origin_geocode = call_api ~configured_address:(Uri.of_string origin_address) in
;; *)