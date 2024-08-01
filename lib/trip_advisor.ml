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

let print_optimal_route ~(origin : Location.t) ~(location_list : Location.t list) ~(day : int) ~distance_data ~travel_method =
  (* print_s [%sexp (graph : Time_ns.Span.t String.Table.t String.Table.t )]; *)
  (* let (best_path, best_time) = Tsp.get_shortest_path ~origin:origin_address ~dest_list:places_list ~path_map:graph in *)
  let best = Tsp.Time_span.get_shortest_path ~origin ~dest_list:location_list ~path_map:distance_data in
  (* print_s [%message (best : (Location.t list * Time_ns.Span.t))]; *)
  print_string ("Day " ^ (Int.to_string day) ^ ": ");
  Google_api.print_maps_address ~directions:(Tuple2.get1 best) ~travel_method;
  return ()
;;

let _run () = 
  (* let date = Date.of_string "2024-09-18" in
  let%bind price = Plane.plane_api ~city_code_origin:"SFO" ~city_code_destination:"NYC" ~date ~desired_info:"price" in
  print_int price; *)

  (* let%bind _airports_list = Parse_csv.read_csv ~filename:"airports.csv" in  *)
  (* let%bind city_codes_list = Parse_csv.read_csv ~filename:"citycodes.csv" in *)
  (* print_s[%message (airports_list : Parse_csv.Row.t list  )]; *)
  (* print_s[%message (city_codes_list : Parse_csv.Row.t list  )]; *)

  let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
  let%bind location_origin_address = Google_api.get_location string_origin_address in

  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind string_places_list = get_desired_places () in
  let%bind location_places_list = Deferred.List.map string_places_list ~how:`Sequential ~f:(fun place ->
    Google_api.get_location place
  ) in
  let all_places = [location_origin_address] @ location_places_list in

  let%bind num_days = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How many days are you traveling?" in
  let num_days = Int.of_string num_days in

  (* let%bind travel_method = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter travel method" in *)
  
  let travel_method_list = ["driving" ; "walking" ; "bicycling" ; "transit"] in
  let travel_method_list = Fzf.Pick_from.Inputs travel_method_list in
  let%bind travel_method =
  (Deferred.repeat_until_finished () (fun () ->
    let%bind choice = (Fzf.pick_one travel_method_list ~header:"Enter travel method" >>| ok_exn) in 
    match choice with
    | Some string -> return (`Finished string)
    | None -> return (`Repeat ())
    ))
  in


  print_endline "Searching Google Maps for travel times...";
  let%bind distance_data = make_destination_graph (List.dedup_and_sort all_places ~compare:Location.compare) travel_method in
  
  print_endline "Computing Optimal Route...";
  if num_days = 1 then 
    let%bind () = print_optimal_route ~origin:location_origin_address ~location_list:location_places_list ~day:1 ~distance_data ~travel_method in
    return ()
  else 
    let clusters = Cluster.k_means_clustering ~k:num_days ~points:location_places_list in 
    let%bind () = Deferred.List.iteri ~how:`Sequential clusters ~f:(fun idx cluster ->
      print_optimal_route ~origin:location_origin_address ~location_list:cluster ~day:(idx + 1) ~distance_data ~travel_method
    ) in
    return ()
;;


let fuzzy_find () = 
  let%bind airports_list = Parse_csv.get_all_airports () in
  let airports_map = List.map airports_list ~f:(fun airport -> 
    Parse_csv.Row.convert_to_string airport, airport) |> String.Map.of_alist_exn in
  let airports_fuzzy_list = Fzf.Pick_from.Map airports_map in

  let%bind airport =
  (Deferred.repeat_until_finished () (fun () ->
    let%bind choice = (Fzf.pick_one airports_fuzzy_list ~case_match:`case_insensitive ~header:"Choose airport" >>| ok_exn) in 
    match choice with
    | Some string -> return (`Finished string)
    | None -> return (`Repeat ())
    ))
  in
  print_string (Parse_csv.Row.convert_to_string airport);

  _run ()
  (* return () *)
;;

let command =
  Command.async
    ~summary:"Calculates Trip Route"
    (let%map_open.Command () = return () in
    fun () -> fuzzy_find ())
;;