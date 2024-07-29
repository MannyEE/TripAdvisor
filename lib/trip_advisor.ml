open! Core
open Async

let rec get_desired_places ((): unit) = 
  let%bind input = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter destination (or ENTER to stop)" in
  match String.equal input "" with 
    | true -> 
      return [];
    | false -> 
      let%map places_list = get_desired_places () in
      input :: places_list
;;

let make_destination_graph (places_list : Location.t list) transport_mode = 
  let map = Location.Table.create () in
  let%bind () = Deferred.List.iter ~how:`Parallel places_list ~f:(fun origin -> 
    let empty_tbl = Hashtbl.find_or_add map origin ~default:Location.Table.create in

    Deferred.List.iter ~how:`Parallel places_list ~f:(fun destination ->
      match [%compare.equal:Location.t] origin destination with 
      | true -> return ()
      | false -> 
        let%bind distance = Google_api.destination_api ~origin ~destination transport_mode in
        Hashtbl.add_exn (empty_tbl) ~key:destination ~data:distance;
        return()
    ) 
  ) in
  return map
;;

let run () = 
  (* let%bind plane = Plane.call_api () in
  Plane.parse_kayak plane; *)
  
  let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
  let%bind location_origin_address = Google_api.get_location string_origin_address in

  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind string_places_list = get_desired_places () in
  
  let%bind location_places_list = Deferred.List.map string_places_list ~how:`Sequential ~f:(fun place ->
    Google_api.get_location place
  ) in
  let all_places = [location_origin_address] @ location_places_list in
    
  let%bind travel_method = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter travel method" in
  let%bind graph = make_destination_graph (List.dedup_and_sort all_places ~compare:Location.compare) travel_method in
  (* print_s [%sexp (graph : Time_ns.Span.t String.Table.t String.Table.t )]; *)

  (* let (best_path, best_time) = Tsp.get_shortest_path ~origin:origin_address ~dest_list:places_list ~path_map:graph in *)
  let best = Tsp.get_shortest_path ~origin:location_origin_address ~dest_list:location_places_list ~path_map:graph in
  print_s [%message (best : (Location.t list * Time_ns.Span.t))];
  Google_api.print_maps_address (Tuple2.get1 best);

  return ()
;;


let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = 
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller"*)
        return ()
      in
     fun () -> 
      run ()
      )
;;


(*
let run = 

      let lat_and_long = 
        Jsonaf.member_exn "results" coordinates 
      |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "geometry"|> Jsonaf.member_exn "location" in
;; *)