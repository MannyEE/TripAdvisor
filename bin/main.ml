open! Core
open Async

let rec get_desired_places ((): unit) = 
  let%bind input = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter destination" in
  match String.equal input "" with 
    | true -> 
      return [];
    | false -> 
      let%map places_list = get_desired_places () in
      input :: places_list
;;

let make_destination_graph places_list transport_mode = 
  let map = String.Table.create () in
  let%bind () = Deferred.List.iter ~how:`Parallel places_list ~f:(fun origin -> 
    let empty_tbl = Hashtbl.find_or_add map origin ~default:String.Table.create in

    Deferred.List.iter ~how:`Parallel places_list ~f:(fun destination ->
      match String.equal origin destination with 
      | true -> return ()
      | false -> 
        let%bind distance = Google_api.destination_api origin destination transport_mode in
        (* let distance = (distance_in_seconds) / 60 in *)
        Hashtbl.add_exn (empty_tbl) ~key:destination ~data:distance;
        return()
    ) 
  ) in
  return map
;;

let run () = 
  let%bind origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind places_list = get_desired_places () in
  let all_places = origin_address :: places_list in

  let%bind travel_method = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter travel method" in
  let%bind graph = make_destination_graph (List.dedup_and_sort all_places ~compare:String.compare) travel_method in

  (* print_s [%sexp (graph : ( string, (string, string) Hashtbl_intf.Hashtbl.t ) Hashtbl_intf.Hashtbl.t)]; *)
  (* print_s [%sexp (graph : Time_ns.Span.t String.Table.t String.Table.t )]; *)

  (* let (best_path, best_time) = Tsp.get_shortest_path ~origin:origin_address ~dest_list:places_list ~path_map:graph in *)
  let best = Tsp.get_shortest_path ~origin:origin_address ~dest_list:places_list ~path_map:graph in

  print_s [%message (best : (string list * Time_ns.Span.t))];
  (* let%bind address1 = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin" in

  let%bind address2 = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter destination" in

  let%bind place_id_origin = Google_api.place_id_api address1 in
  let%bind place_id_destination = Google_api.place_id_api address2 in
  
  let%bind distance_in_seconds = Google_api.destination_api place_id_origin place_id_destination travel_method in

  let distance = (Int.of_string distance_in_seconds) / 60 in *)
  
  (* let () = print_endline ((Int.to_string distance) ^ " minutes " ^ travel_method) in *)
  return ();
;;


let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller"*)
      in
     fun () -> 

      run ()

      )
;;

let () = Command_unix.run command_play;;


(*
let run = 
  let location_list = get_desired_places () in
  let origin = List.hd_exn location_list in
  let coordinate_list = List.map (List.tl_exn location_list) ~f:(fun destination -> 
    (lat, long)) in

  let coordinate_list = List.map (List.tl_exn location_list) ~f:(fun destination -> 
    (lat, long)) in

  let coordinate_list = List.map (List.tl_exn location_list) ~f:(fun destination -> 
    (* Location.find_distance  *)
    (lat, long)) in


  (* let%bind x = Google_api.geocode in
      let%bind y = Google_api.destination in
      let coordinates = Jsonaf.of_string x in
      let direction_info = Jsonaf.of_string y in
      
      let lat_and_long = 
        Jsonaf.member_exn "results" coordinates 
      |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "geometry"|> Jsonaf.member_exn "location" in

      let lat = Jsonaf.float_exn (Jsonaf.member_exn "lat" lat_and_long) in
      let long = Jsonaf.float_exn (Jsonaf.member_exn "lng" lat_and_long) in
      print_endline (Float.to_string lat);
      print_endline (Float.to_string long);

      let direction_info = 
        Jsonaf.member_exn "routes" direction_info |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in
      let duration = Jsonaf.int_exn direction_info in
      
      print_endline (Int.to_string duration); *)
;; *)