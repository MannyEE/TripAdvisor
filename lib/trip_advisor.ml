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


let print_optimal_google_route ~(day : int) ~(best_route : Location.t list) ~travel_method =
  print_endline ("\nDay " ^ (Int.to_string day) ^ ": ");
  print_endline (String.concat ~sep:" -> " (List.map best_route ~f:(fun location -> location.name)));
  Google_api.print_maps_address ~directions:(best_route) ~travel_method;
;;



let fzf_choose_between ~(options_list : string list) ~(message : string)  =
  let options_list = Fzf.Pick_from.Inputs options_list in
  (Deferred.repeat_until_finished () (fun () ->
    let%bind choice = (Fzf.pick_one options_list ~header:message >>| ok_exn) in 
    match choice with
    | Some string -> return (`Finished string)
    | None -> return (`Repeat ())
  ))
;;

let find_origin_airport () = 
  let%bind airports_list = Parse_csv.get_all_airports () in
  let airports_map = List.map airports_list ~f:(fun airport -> 
    Airport.convert_to_string airport, airport) |> String.Map.of_alist_exn in
  let airports_fuzzy_list = Fzf.Pick_from.Map airports_map in
  let%bind airport =
  (Deferred.repeat_until_finished () (fun () ->
    let%bind choice = (Fzf.pick_one airports_fuzzy_list ~case_match:`case_insensitive ~tiebreak:(Nonempty_list.of_list_exn ([Fzf.Tiebreak.Begin])) ~header:"Choose origin airport" >>| ok_exn) in 
    match choice with
    | Some airport -> return (`Finished airport)
    | None -> 
      ( print_string "Trying to quit";
        return (`Repeat ()))
    ))
  in
  return airport
;;

let find_destination_airports origin_str = 
  let%bind airports_list = Parse_csv.get_all_airports () in
  let airports_map = List.map airports_list ~f:(fun airport -> 
    Airport.convert_to_string airport, airport) |> String.Map.of_alist_exn in
  let airports_fuzzy_list = Fzf.Pick_from.Map airports_map in

  Deferred.repeat_until_finished [] (fun cur_list ->
    let cur_route = String.concat ~sep:", " (List.map cur_list ~f:(fun airport -> Airport.(airport.name) ^ " (" ^ airport.code ^ ")")) in

    let%bind choice = (Fzf.pick_one airports_fuzzy_list ~case_match:`case_insensitive ~tiebreak:(Nonempty_list.of_list_exn ([Fzf.Tiebreak.Begin])) ~header:("Choose destination airports (ESC to quit)\nOrigin: " ^ origin_str ^ " | Current Route: " ^ cur_route) >>| ok_exn) in 
    match choice with
    | Some airport -> (
      let new_list = cur_list @ [airport] in
      return (`Repeat new_list)
    )
    | None -> (
      if List.length cur_list < 1 then (return (`Repeat cur_list) )
      else
        return (`Finished cur_list)
    )
    )
  
  (* return () *)
;;

let intracity_optimization map = 
  let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
  let%bind location_origin_address = Google_api.get_location string_origin_address in
  (* print_s [%message (location_origin_address.coordinates : Location.Coordinates.t)]; *)

  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind string_places_list = get_desired_places () in
  let%bind location_places_list = Deferred.List.map string_places_list ~how:`Sequential ~f:(fun place ->
    Google_api.get_location place
  ) in

(* RECOMMENDATION STUFF *)
(* let%bind recommend_str = Recommend.find_nearby ~recommendation_type:"restaurant" ~location:location_origin_address in
print_endline recommend_str; *)

(* *)

  let%bind num_days = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How many days are you traveling?" in
  let num_days = Int.of_string num_days in

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

  let all_places = [location_origin_address] @ location_places_list in
    
    print_endline "Searching Google Maps for travel times...";
    let%bind distance_data = Tsp.Intra_city_duration.make_destination_graph map (List.dedup_and_sort all_places ~compare:Location.compare) travel_method in
    
    print_endline "Computing Optimal Route...";
    
    let%bind best = 
      (match num_days with
      | 1 -> 
        let best_route = Tsp.Intra_city_duration.get_shortest_path ~origin:location_origin_address ~dest_list:location_places_list ~path_map:distance_data in
        return [best_route]
      | _ ->
        let clusters = Cluster.k_means_clustering ~k:num_days ~points:location_places_list in 
        let%bind best_routes = Deferred.List.map ~how:`Sequential clusters ~f:(fun cluster -> return (Tsp.Intra_city_duration.get_shortest_path ~origin:location_origin_address ~dest_list:cluster ~path_map:distance_data)) in
        return best_routes) in


    let () = List.iteri best ~f:(fun idx route->
        print_optimal_google_route ~day:(idx + 1) ~best_route:(fst route) ~travel_method) in

    return ()

    



    
;;

let filter_map map = 

  Hashtbl.iter map ~f:(fun inner_tbl ->
    Hashtbl.filter_inplace inner_tbl ~f:(fun data -> not (Kayak_data.is_large data)))
  ;;


let run () = 
  (* let date = Date.of_string "2024-09-18" in
  let%bind price = Plane.plane_api ~city_code_origin:"SFO" ~city_code_destination:"NYC" ~date ~desired_info:"price" in
  print_int price; *)

  (* let%bind flying = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Do you plan on flying to your destinations" in *)
  (* let filename = "kayak_data_saver" in
  let%bind map_option = Reader.load_sexp filename [%of_sexp : Kayak_data.t Airport.Table.t Airport.Table.t] in
  let map =
  match map_option with 
    | Ok map -> map 
    | Error _-> Airport.Table.create () in

  
  let hashtbl = Hashtbl.map map ~f:(fun inner_tbl ->
    Hashtbl.map inner_tbl ~f:(fun data ->
      Kayak_data.{price = data.price; duration = data.duration; flight_date = Date.of_string "20241212"; request_date = Date.of_string "20241212"} )) in

  let%bind () = Writer.save_sexp filename [%sexp (hashtbl : Kayak_data.t2 Airport.Table.t Airport.Table.t)] in

  return () *)
  


  let flying_options_list = [ "No" ; "Yes"] in
  let%bind flying = fzf_choose_between ~options_list:flying_options_list ~message:"Do you plan on flying to your destinations" in

  match flying with
  | "Yes" ->
    let optimize_options_list = ["time" ; "price"] in 
    let%bind desired_optimization = fzf_choose_between ~options_list:optimize_options_list ~message:"How should we optimize your flights?" in

    (* let%bind departure_date = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "What day would you like to leave (Enter in YYYYMMDD)" >>| Date.of_string in *)
    let%bind date_list = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "What day would you like to leave (Enter in MM/DD/YYYY)" >>| 
    String.split ~on:'/' in
    let departure_date = 
    Date.of_string ((List.nth_exn date_list 2) ^ (List.nth_exn date_list 0) ^ (List.nth_exn date_list 1)) in

    (* DONT WE WANT THIS FOR FLIGHT DATES?*)
    let%bind stay_length = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How many days would you like to stay in each city?" >>| Int.of_string in
    let%bind airport_origin_address = find_origin_airport () in
    (* print_s [%message (airport_origin_address : Airport.t)]; *)

    print_endline "What places would you like to visit? Put in one address at a time";

    let%bind cities = (find_destination_airports (Airport.(airport_origin_address.name) ^ " (" ^ airport_origin_address.code ^ ")")) in

    let all_places = [airport_origin_address] @ cities in
    let sorted_city_list = List.dedup_and_sort all_places ~compare:Airport.compare in
    print_endline "Searching for travel Info...";

 (* UNCOMMENT FOR ACTUAL RESULTS *)

    let filename = "kayak_data_saver" in
    let%bind map_option = Reader.load_sexp filename [%of_sexp : Kayak_data.t Airport.Table.t Airport.Table.t] in
    let map = 
    match map_option with 
    | Ok map -> map 
    | Error _-> Airport.Table.create () in

    let%bind (distance_data) = Tsp.Flight_duration.make_destination_graph map (sorted_city_list) departure_date in

    let%bind optimal_route = 
    (match desired_optimization with 
      | "time" -> 
        print_endline "Computing Optimal Route...";
        let best = Tsp.Flight_duration.get_shortest_path ~origin:airport_origin_address ~dest_list:cities ~path_map:distance_data in
        return (fst best)
        
      | "price" ->
        print_endline "Computing Optimal Route...";
        let best = Tsp.Flight_prices.get_shortest_path ~origin:airport_origin_address ~dest_list:cities ~path_map:distance_data in
        return (fst best)
      | _ -> assert false) in

    filter_map distance_data;
    let%bind () = Writer.save_sexp filename [%sexp (distance_data : Kayak_data.t Airport.Table.t Airport.Table.t)] in

    (* print_s [%sexp (optimal_route: Airport.t list)]; *)
    print_string "\nFlight path: ";
    print_endline ((String.concat ~sep:" -> " (List.map optimal_route ~f:(fun location -> location.name))) ^ "\n");

    let kayak_route_link = Plane.get_kayak_link ~best_route:optimal_route ~departure_date ~stay_length in

    print_endline kayak_route_link;
    print_newline ();
    let%bind optimize_cities = fzf_choose_between ~options_list:flying_options_list ~message:"Would you like to optimize your daily trips in each city as well?" in

    let%bind () =
      match optimize_cities with
      | "Yes"-> 
        Deferred.List.iter optimal_route ~how:`Sequential ~f:(fun city ->
          let () = print_endline ("Optimize your trip in: " ^ (city.name)) in
          let map = Location.Table.create () in
          let%bind () = intracity_optimization map in
          return ()
        ) 
  
      | "No"-> 
        return ()
      | _ -> assert false
      in

    return ()

  | "No" ->
    let map = Location.Table.create () in
    intracity_optimization map
  | _ -> assert false
;;


let command =
  Command.async
    ~summary:"Calculates Trip Route"
    (let%map_open.Command () = return () in
    fun () -> run ())
;;