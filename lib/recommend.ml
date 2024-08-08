open! Core
open Async
open! Cohttp
open Cohttp_async
let api = Lazy_deferred.create (fun () -> Reader.file_contents "/home/ubuntu/api" );;

let call_api ~configured_address =
  let%bind key = Lazy_deferred.force_exn api in

  let uri_with_key = Uri.add_query_param' configured_address ("key" , key) in
  (* let configured_address = configured_address ^ "&key=" ^ key in *)
  (* let%bind configured_address = config_geocode_address ~street_address in *)
  (* print_endline configured_address; *)
  Client.get (uri_with_key) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  return (Jsonaf.of_string body)
;;
let find_nearby = 

;;