open! Core 
open Async


(* Old function: gets random elems *)
let rec get_n_random_elems ~k ~(points : Location.t list) : Location.Coordinates.t list = 
  if k = 0 then [] 
  else 
    let random_elem = List.random_element_exn ~random_state:Random.State.default points in
    let new_points = (List.filter points ~f:(fun point -> 
      match Location.compare point random_elem with 
      | 0 -> false 
      | _ -> true 
    ) ) in
  List.append [random_elem.coordinates] (get_n_random_elems ~k:(k - 1) ~points:new_points)
;;

let distance_between (point1 : Location.Coordinates.t) (point2 : Location.Coordinates.t) = 
  let long1 = point1.long in 
  let long2 = point2.long in
  let lat1 = point1.lat in 
  let lat2 = point2.lat in 
  Float.sqrt ((Float.square (long2 -. long1)) +. (Float.square (lat2 -. lat1)))
;;

let calculate_centroid (cluster : Location.t Deque.t) : Location.Coordinates.t = 
  let (lat, long) = Deque.fold cluster ~init:(0.0, 0.0) ~f:(fun (cur_lat, cur_long) location -> 
    (cur_lat +. (location.coordinates.lat), cur_long +. (location.coordinates.long))
  ) in
  let num_elems = Float.of_int (Deque.length cluster) in
  Location.Coordinates.{lat = (lat /. num_elems) ; long = (long /. num_elems)}
;;

let find_closest_centroid ~(coords : Location.Coordinates.t) ~(clusters : (Location.Coordinates.t * Location.t Deque.t) list) = 
  List.fold clusters ~init:(Deque.create (), Float.max_value) ~f:(fun (best_deque, shortest_dist) (check_coords, deque) ->
    let check_dist = distance_between coords check_coords in 
    if (Float.(<.) check_dist shortest_dist) then (deque, check_dist) else
      (best_deque, shortest_dist)
  ) |> fst
;;

let rec k_means_impl ~k ~(points : Location.t list) ~(old_centroids : Location.Coordinates.t list) : Location.t Deque.t list =
  let new_clusters = List.map old_centroids ~f:(fun centroid -> (centroid, Deque.create ()) )in
  List.iter points ~f:(fun point ->
    let coords = point.coordinates in
    let closest_deque = find_closest_centroid ~coords ~clusters:new_clusters in 
    Deque.enqueue_back closest_deque point;
  );
  let new_clusters = List.map new_clusters ~f:snd in
  let new_centroids = List.map new_clusters ~f:(fun cluster -> calculate_centroid cluster) in
  if (List.equal 
    [%compare.equal:Location.Coordinates.t]
   new_centroids old_centroids
  ) then
    new_clusters
  else 
    k_means_impl ~k ~points ~old_centroids:new_centroids
;;
(* 
let get_closest_centroid_distance ~(location : Location.t) ~(centroid_list : Location.Coordinates.t list) =
  List.fold centroid_list ~init:(Float.max_value) ~f:(fun )
;; *)
(* 
let rec k_means_initialize_rec ~k ~(points : Location.t list) ~(centroid_list : Location.Coordinates.t list) : Location.Coordinates.t list = 
  if k = 0 then centroid_list 
  else 


    let new_centroid : Location.t = 
      List.fold points ~init:(List.hd_exn points) ~f:(fun (x) location -> 
          location
        )
    in
    let new_points = (List.filter points ~f:(fun point -> 
      match Location.compare point new_centroid with 
      | 0 -> false 
      | _ -> true 
    ) ) in
  k_means_initialize_rec ~k:(k - 1) ~points:new_points ~centroid_list:(centroid_list @ (new_centroid.))
;;

let k_means_initialize ~k ~(points : Location.t list) : Location.Coordinates.t list = 
    let random_elem = List.random_element_exn ~random_state:Random.State.default points in
    let new_points = (List.filter points ~f:(fun point -> 
      match Location.compare point random_elem with 
      | 0 -> false 
      | _ -> true 
    ) ) in
    k_means_initialize_rec ~k:(k - 1) ~points:new_points ~centroid_list:[random_elem.coordinates]
;; *)


let k_means_clustering ~k ~points =
  let old_centroids = get_n_random_elems ~k ~points in
  (* print_s [%message (old_centroids : Location.Coordinates.t list)]; *)
  let cluster_deque_list = k_means_impl ~k ~points ~old_centroids in
  List.map cluster_deque_list ~f:(fun deque -> Deque.to_list deque)
;;

(* 
let plan_multi_trip =
  let%bind num_trip_days = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "How long is your stay?" in
  let%bind string_origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in
  let%bind location_origin_address = Google_api.get_location string_origin_address in

  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind string_places_list = Trip_advisor.get_desired_places () in



;; *)

let create_empty_coordinate_location ~(lat : float) ~(long : float) = 
  Location.{
    place_id = "" ;
    name = "" ;
    formatted_address = "" ;
    coordinates =  Coordinates.{lat ; long}
  }
;;


let%expect_test "testing k means cluster" = 
  let (loc_list : Location.t list) = [
    (create_empty_coordinate_location ~lat:2.0 ~long:1.0) ;
    (create_empty_coordinate_location ~lat:1.0 ~long:2.0) ;
    (create_empty_coordinate_location ~lat:1.0 ~long:1.0) ;
    (create_empty_coordinate_location ~lat:(-2.0) ~long:(-1.0)) ;
    (create_empty_coordinate_location ~lat:(-1.0) ~long:(-2.0));
    (create_empty_coordinate_location ~lat:(-1.0) ~long:(-1.0)) ;
  ] in
  let clusters = k_means_clustering ~points:loc_list ~k:3 in
  let (clusters_coords : Location.Coordinates.t list list) = 
    List.map clusters ~f:(fun cluster ->
        List.map cluster ~f:(fun location -> location.coordinates)
    )
  in
  print_s [%message (clusters_coords : Location.Coordinates.t list list)];
  return [%expect {|
    (clusters_coords
     ((((lat 2) (long 1)) ((lat 1) (long 1)))
      (((lat -2) (long -1)) ((lat -1) (long -2)) ((lat -1) (long -1)))
      (((lat 1) (long 2))))) |}]
;;
  