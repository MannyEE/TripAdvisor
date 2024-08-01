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

let print_optimal_google_route ~(day : int) (best_route) =
  print_string ("Day " ^ (Int.to_string day) ^ ": ");
  Google_api.print_maps_address (best_route);
  return ()
;;

let run () = 
  (* let date = Date.of_string "2024-09-18" in
  let%bind price = Plane.plane_api ~city_code_origin:"SFO" ~city_code_destination:"NYC" ~date ~desired_info:"price" in
  print_int price; *)

  let%bind flying = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Do you plan on flying to your destinations" in

  match flying with
  | "Yes" ->
    let%bind desired_optimization = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Should we optimize your flights for time or price?" in (*Should only be able to put time or price*)
    let%bind _departure_date = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "What day would you like to leave (Enter in YYYYMMDD)" in
    let%bind _stay_length = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How many days would you like to stay in each city?" in
    let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
    let origin_area_code = Plane.get_airport_code string_origin_address in
    print_endline "What places would you like to visit? Put in one address at a time";
    let%bind cities = get_desired_places () in
    let area_codes = List.map cities ~f:(fun location -> Plane.get_airport_code location) in
    let all_places = [origin_area_code] @ area_codes in
    let sorted_city_list = List.dedup_and_sort all_places ~compare:Airport_code.compare in
    print_endline "Searching for travel Info...";
    let date = Date.of_string "20001212" in


    print_endline "Computing Optimal Route...";
    let%map _optimal_route = 
    (match desired_optimization with 
      | "time" -> 
        let%map (distance_data) = Tsp.Flight_duration.make_destination_graph (sorted_city_list) date in
        let best = Tsp.Flight_duration.get_shortest_path ~origin:origin_area_code ~dest_list:cities ~path_map:distance_data in
        fst best
        
      | "price" ->
        let%map (distance_data) = Tsp.Flight_prices.make_destination_graph (sorted_city_list) date in
        let best = Tsp.Flight_prices.get_shortest_path ~origin:origin_area_code ~dest_list:cities ~path_map:distance_data in
        fst best
      | _ -> assert false) in

    


    ()
    (* List.iter dis *)



  | "No" ->
  
    let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
    let%bind location_origin_address = Google_api.get_location string_origin_address in

    print_endline "What places would you like to visit? Put in one address at a time";
    let%bind string_places_list = get_desired_places () in
    let%bind location_places_list = Deferred.List.map string_places_list ~how:`Sequential ~f:(fun place ->
      Google_api.get_location place
    ) in

    let%bind num_days = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How many days are you traveling?" in
    let num_days = Int.of_string num_days in

    let%bind travel_method = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter travel method" in
    let all_places = [location_origin_address] @ location_places_list in
    
    print_endline "Searching Google Maps for travel times...";
    let%bind distance_data = Tsp.Intra_city_duration.make_destination_graph (List.dedup_and_sort all_places ~compare:Location.compare) travel_method in
    
    print_endline "Computing Optimal Route...";

    
    if num_days = 1 then 
      let best = Tsp.Intra_city_duration.get_shortest_path ~origin:location_origin_address ~dest_list:location_places_list ~path_map:distance_data in
      let%bind () = print_optimal_google_route ~day:1 (fst best) in
      return ()
    else 
      let clusters = Cluster.k_means_clustering ~k:num_days ~points:location_places_list in 
      let%bind () = Deferred.List.iteri ~how:`Sequential clusters ~f:(fun idx cluster ->
        let best = Tsp.Intra_city_duration.get_shortest_path ~origin:location_origin_address ~dest_list:cluster ~path_map:distance_data in
        print_optimal_google_route ~day:(idx + 1) (fst best);
      ) in
    return ()
  | _ -> assert false
;;

let command =
  Command.async
    ~summary:"Calculates Trip Route"
    (let%map_open.Command () = return () in
    fun () -> run ())
;;