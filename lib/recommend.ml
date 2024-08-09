(* open! Core
open Async
open! Cohttp
open Cohttp_async

let create_recommendation_header key = 
  Cohttp.Header.of_list [("X-Goog-Api-Key", key) ; ("Content-Type", "application/json") ; ("X-Goog-FieldMask", "places.displayName")]
;;

let api = Lazy_deferred.create (fun () -> Reader.file_contents "/home/ubuntu/api" );;
let call_api ~git add -Aconfigured_address =
  let%bind key = Lazy_deferred.force_exn api in

  let uri_with_key = Uri.add_query_param' configured_address ("key" , key) in
  Client.post (uri_with_key) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  return (Jsonaf.of_string body)
;;



let create_body ~recommendation_type ~(location : Location.t)= 
  Cohttp_async.Body.of_string([%string {|{"includedTypes": ["%{recommendation_type}"],
    "maxResultCount": 10,
    "locationRestriction": {
      "circle": {
        "center": {
          "latitude": %{location.coordinates.lat#Float},
          "longitude": %{location.coordinates.long#Float}},
        "radius": 500.0"}}}|}])
;;

let call_api ~address ~key ~recommendation_type ~location =
  Client.post ~body:(create_body ~recommendation_type ~location) ~headers:(create_recommendation_header key) (Uri.of_string (address)) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  (* print_endline body; *)
  return body
;;
let find_nearby ~recommendation_type ~location = 
  let%bind key = Lazy_deferred.force_exn api in
  let address = "https://places.googleapis.com/v1/places:searchNearby" in
  let%map response = call_api ~address ~key ~recommendation_type ~location in
  response
;; *)