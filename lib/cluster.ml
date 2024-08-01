open! Core 
open Async


(* Old function: gets random elems *)
(* let rec k_means_initialize ~k ~(points : Location.t list) : Location.Coordinates.t list = 
  if k = 0 then [] 
  else 
    let random_elem = List.random_element_exn ~random_state:Random.State.default points in
    let new_points = (List.filter points ~f:(fun point -> 
      match Location.compare point random_elem with 
      | 0 -> false 
      | _ -> true 
    ) ) in
  List.append [random_elem.coordinates] (k_means_initialize ~k:(k - 1) ~points:new_points)
;; *)

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

let get_closest_centroid_distance ~(location : Location.t) ~(centroid_list : Location.Coordinates.t list) =
  List.fold centroid_list ~init:(Float.max_value) ~f:(fun cur_min_dist centroid ->
    let check_dist = distance_between location.coordinates centroid in 
    if (Float.(<) check_dist cur_min_dist) then check_dist 
    else cur_min_dist
  )
;; 

let filter_out_point ~(points : Location.t list) ~point = 
  List.filter points ~f:(fun check_point -> 
    match Location.compare point check_point with 
    | 0 -> false 
    | _ -> true 
  ) 

;;

let rec k_means_initialize ~k ~(points : Location.t list) ~(centroid_list : Location.Coordinates.t list) : Location.Coordinates.t list = 
  if k = 0 then centroid_list 
  else 
    let new_centroid = 
      (* Finds the point whose closest centroid is farthest away *)
      List.fold points ~init:(List.hd_exn points, Float.max_value) ~f:(fun (farthest_centroid, farthest_dist) location -> 
        let check_dist = get_closest_centroid_distance ~location ~centroid_list in
        if (Float.(>) check_dist farthest_dist) then (location, check_dist) 
        else (farthest_centroid, farthest_dist)
      ) |> fst
    in

    let new_points = filter_out_point ~points ~point:new_centroid in
  k_means_initialize ~k:(k - 1) ~points:new_points ~centroid_list:(centroid_list @ [(new_centroid.coordinates)])
;;

let k_means_clustering ~k ~points =
  let initial_centroid = List.random_element_exn ~random_state:Random.State.default points in
  let new_initialize_list = filter_out_point ~points ~point:initial_centroid in
  let initial_centroids = k_means_initialize ~k:(k - 1) ~points:(new_initialize_list) ~centroid_list:[initial_centroid.coordinates] in
  (* print_s [%message (old_centroids : Location.Coordinates.t list)]; *)
  let cluster_deque_list = k_means_impl ~k ~points ~old_centroids:initial_centroids in
  List.map cluster_deque_list ~f:(fun deque -> Deque.to_list deque)
;;

let create_empty_coordinate_location ~(lat : float) ~(long : float) = 
  Location.{
    place_id = "" ;
    name = "" ;
    formatted_address = "" ;
    coordinates =  Coordinates.{lat ; long};
    airport_code = ""
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
  